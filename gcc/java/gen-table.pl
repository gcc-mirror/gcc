#! /usr/bin/perl

#    Copyright (C) 2000, 2001, 2003 Free Software Foundation

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2, or (at your option)
#    any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#    02110-1301, USA.

# gen-table.pl - Generate tables for gcj from Unicode data.
# Usage: perl gen-table.pl DATA-FILE
#
# You can find the Unicode data file here:
#   ftp://www.unicode.org/Public/3.0-Update1/UnicodeData-3.0.1.txt
# Please update this URL when this program is used with a more
# recent version of the table.  Note that this table cannot be
# distributed with gcc.
# This program should not be re-run indiscriminately.  Care must be
# taken that what it generates is in sync with the Java specification.

# Names of fields in Unicode data table.
$CODE = 0;
$NAME = 1;
$CATEGORY = 2;
$COMBINING_CLASSES = 3;
$BIDI_CATEGORY = 4;
$DECOMPOSITION = 5;
$DECIMAL_VALUE = 6;
$DIGIT_VALUE = 7;
$NUMERIC_VALUE = 8;
$MIRRORED = 9;
$OLD_NAME = 10;
$COMMENT = 11;
$UPPER = 12;
$LOWER = 13;
$TITLE = 14;

# Start of special-cased gaps in Unicode data table.
%gaps = (
	 0x4e00 => "CJK",
	 0xac00 => "Hangul",
	 0xd800 => "Unassigned High Surrogate",
	 0xdb80 => "Private Use High Surrogate",
	 0xdc00 => "Low Surrogate",
	 0xe000 => "Private Use"
	 );

# This lists control characters which are also considered whitespace.
# This is a somewhat odd list, taken from the JCL definition of
# Character.isIdentifierIgnorable.
%whitespace_controls =
    (
     0x0009 => 1,
     0x000a => 1,
     0x000b => 1,
     0x000c => 1,
     0x000d => 1,
     0x001c => 1,
     0x001d => 1,
     0x001e => 1,
     0x001f => 1
     );

open (INPUT, "< $ARGV[0]") || exit 1;

$last_code = -1;
while (<INPUT>)
{
    chop;
    @fields = split (';', $_, 30);
    if ($#fields != 14)
    {
	print STDERR "Entry for $fields[$CODE] has wrong number of fields\n";
    }

    $code = hex ($fields[$CODE]);
    last if $code > 0xffff;
    if ($code > $last_code + 1)
    {
	# Found a gap.
	if (defined $gaps{$code})
	{
	    # Fill the gap with the last character read.
	    @gfields = @fields;
	}
	else
	{
	    # The gap represents undefined characters.  Only the type
	    # matters.
	    @gfields = ('', '', 'Cn', '0', '', '', '', '', '', '', '',
			'', '', '', '');
	}
	for (++$last_code; $last_code < $code; ++$last_code)
	{
	    $gfields{$CODE} = sprintf ("%04x", $last_code);
	    &process_one ($last_code, @gfields);
	}
    }
    &process_one ($code, @fields);
    $last_code = $code;
}

close (INPUT);

@gfields = ('', '', 'Cn', '0', '', '', '', '', '', '', '',
	    '', '', '', '');
for (++$last_code; $last_code < 0x10000; ++$last_code)
{
    $gfields{$CODE} = sprintf ("%04x", $last_code);
    &process_one ($last_code, @gfields);
}
--$last_code;			# Want last to be 0xFFFF.

&print_tables ($last_code);

exit 0;

# Process a single character.
sub process_one
{
    my ($code, @fields) = @_;

    my @value = ();
    my $type = $fields[$CATEGORY];

    # See if the character is a valid identifier start.
    if ($type =~ /L./		# Letter
	|| $type eq 'Pc'	# Connecting punctuation
	|| $type eq 'Sc')	# Currency symbol
    {
	push (@value, 'LETTER_START');
    }

    # See if the character is a valid identifier member.
    if ($type =~ /L./		# Letter
	|| $type eq 'Pc'	# Connecting punctuation
	|| $type eq 'Sc'	# Currency symbol
	|| $type =~ /N[dl]/	# Number: decimal or letter
	|| $type =~ /M[nc]/	# Mark: non-spacing or combining
	|| ($type eq 'Cc'	# Certain controls
	    && ! defined $whitespace_controls{$code})
	|| ($code >= 0x200c	# Join controls
	    && $code <= 0x200f)
	|| ($code >= 0x202a	# Bidi controls -- note that there
				# is a typo in the JCL where these are
				# concerned.
	    && $code <= 0x202e)
	|| ($code >= 0x206a	# Format controls
	    && $code <= 0x206f)
	|| $code == 0xfeff)	# ZWNBSP
    {
	push (@value, 'LETTER_PART');
    }

    if (($type =~ /Z./
	 # Java treats some values specially as non-spaces.
	 && $code != 0x00a0
	 && $code != 0x2007
	 && $code != 0x202f)
	# And for our purposes there are some that should be specially
	# treated as spaces.
	|| $code == 0x000b
	|| ($code >= 0x001c && $code <= 0x001f))
    {
	push (@value, 'LETTER_SPACE');
    }

    if (! @value)
    {
	$value = '0';
    }
    else
    {
	$value = '(' . join (' | ', @value) . ')';
    }

    $map[$code] = $value;
}

sub print_tables
{
    my ($last) = @_;

    local ($bytes_out) = 0;

    open (OUT, "> chartables.h");

    print OUT "/* This file is automatically generated.  DO NOT EDIT!\n";
    print OUT "   Instead, edit gen-table.pl and re-run.  */\n\n";

    print OUT "#ifndef GCC_CHARTABLES_H\n";
    print OUT "#define GCC_CHARTABLES_H\n\n";

    print OUT "#define LETTER_START 1\n";
    print OUT "#define LETTER_PART  2\n";
    print OUT "#define LETTER_SPACE 4\n\n";
    print OUT "#define LETTER_MASK  7\n\n";

    for ($count = 0; $count <= $last; $count += 256)
    {
	$row[$count / 256] = &print_row ($count, '(char *) ', 'const char', 1,
					 'page');
    }

    print OUT "static const char *const type_table[256] = {\n";
    for ($count = 0; $count <= $last; $count += 256)
    {
	print OUT ",\n" if $count > 0;
	print OUT "  ", $row[$count / 256];
	$bytes_out += 4;
    }
    print OUT "\n};\n\n";

    print OUT "#endif /* ! GCC_CHARTABLES_H */\n";

    close (OUT);

    printf "Generated %d bytes\n", $bytes_out;
}

# Print a single "row" of a two-level table.
sub print_row
{
    my ($start, $def_pfx, $typname, $typsize, $name) = @_;

    my ($i);
    my (@values);
    my ($flag) = 1;
    my ($off);
    for ($off = 0; $off < 256; ++$off)
    {
	$values[$off] = $map[$off + $start];
	if ($values[$off] ne $values[0])
	{
	    $flag = 0;
	}
    }
    if ($flag)
    {
	return $def_pfx . $values[0];
    }

    printf OUT "static %s %s%d[256] = {\n  ", $typname, $name, $start / 256;
    my ($column) = 2;
    for ($i = $start; $i < $start + 256; ++$i)
    {
	print OUT ", "
	    if $i > $start;
	my ($text) = $values[$i - $start];
	if (length ($text) + $column + 2 > 78)
	{
	    print OUT "\n  ";
	    $column = 2;
	}
	print OUT $text;
	$column += length ($text) + 2;
    }
    print OUT "\n};\n\n";

    $bytes_out += 256 * $typsize;

    return sprintf "%s%d", $name, $start / 256;
}
