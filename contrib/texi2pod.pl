#! /usr/bin/perl -w

#   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

# This file is part of GNU CC.

# GNU CC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston MA 02111-1307, USA.

# This does trivial (and I mean _trivial_) conversion of Texinfo
# markup to Perl POD format.  It's intended to be used to extract
# something suitable for a manpage from a Texinfo document.

$output = 0;
$skipping = 0;
%sects = ();
$section = "";
@icstack = ();
@endwstack = ();
@skstack = ();
$shift = "";
%defs = ();
$fnno = 1;

while ($_ = shift) {
    if (/^-D(.*)$/) {
	if ($1 ne "") {
	    $flag = $1;
	} else {
	    $flag = shift;
	}
	die "no flag specified for -D\n"
	    unless $flag ne "";
	die "flags may only contain letters, digits, hyphens, and underscores\n"
	    unless $flag =~ /^[a-zA-Z0-9_-]+$/;
	$defs{$flag} = "";
    } elsif (/^-/) {
	usage();
    } else {
	$in = $_, next unless defined $in;
	$out = $_, next unless defined $out;
	usage();
    }
}

if (defined $in) {
    open(STDIN, $in) or die "opening \"$in\": $!\n";
}
if (defined $out) {
    open(STDOUT, ">$out") or die "opening \"$out\": $!\n";
}

while(<STDIN>)
{
    # Certain commands are discarded without further processing.
    /^\@(?:
	 [a-z]+index		# @*index: useful only in complete manual
	 |need			# @need: useful only in printed manual
	 |(?:end\s+)?group	# @group .. @end group: ditto
	 |page			# @page: ditto
	 |node			# @node: useful only in .info file
	)\b/x and next;
    
    chomp;

    # Look for filename and title markers.
    /^\@setfilename\s+([^.]+)/ and $fn = $1, next;
    /^\@settitle\s+([^.]+)/ and $tl = $1, next;

    # Look for blocks surrounded by @c man begin SECTION ... @c man end.
    # This really oughta be @ifman ... @end ifman and the like, but such
    # would require rev'ing all other Texinfo translators.
    /^\@c man begin ([A-Z]+)/ and $sect = $1, $output = 1, next;
    /^\@c man end/ and do {
	$sects{$sect} = "" unless exists $sects{$sect};
	$sects{$sect} .= postprocess($section);
	$section = "";
	$output = 0;
	next;
    };
    next unless $output;

    # Discard comments.  (Can't do it above, because then we'd never see
    # @c man lines.)
    /^\@c\b/ and next;

    # End-block handler goes up here because it needs to operate even
    # if we are skipping.
    /^\@end\s+([a-z]+)/ and do {
	# Ignore @end foo, where foo is not an operation which may
	# cause us to skip, if we are presently skipping.
	my $ended = $1;
	next if $skipping && $ended !~ /^(?:ifset|ifclear|ignore|menu)$/;

	die "\@end $ended without \@$ended at line $.\n" unless defined $endw;
	die "\@$endw ended by \@end $ended at line $.\n" unless $ended eq $endw;

	$endw = pop @endwstack;

	if ($ended =~ /^(?:ifset|ifclear|ignore|menu)$/) {
	    $skipping = pop @skstack;
	    next;
	} elsif ($ended =~ /^(?:example|smallexample)$/) {
	    $shift = "";
	    $_ = "";	# need a paragraph break
	} elsif ($ended =~ /^(?:itemize|enumerate|table)$/) {
	    $_ = "\n=back\n";
	    $ic = pop @icstack;
	} else {
	    die "unknown command \@end $ended at line $.\n";
	}
    };

    # We must handle commands which can cause skipping even while we
    # are skipping, otherwise we will not process nested conditionals
    # correctly.
    /^\@ifset\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifset";
	$skipping = 1 unless exists $defs{$1};
	next;
    };

    /^\@ifclear\s+([a-zA-Z0-9_-]+)/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = "ifclear";
	$skipping = 1 if exists $defs{$1};
	next;
    };

    /^\@(ignore|menu)\b/ and do {
	push @endwstack, $endw;
	push @skstack, $skipping;
	$endw = $1;
	$skipping = 1;
	next;
    };

    next if $skipping;

    # Character entities.  First the ones that can be replaced by raw text
    # or discarded outright:
    s/\@copyright\{\}/(c)/g;
    s/\@dots\{\}/.../g;
    s/\@enddots\{\}/..../g;
    s/\@([.!? ])/$1/g;
    s/\@[:-]//g;
    s/\@bullet(?:\{\})?/*/g;
    s/\@TeX\{\}/TeX/g;
    s/\@pounds\{\}/\#/g;
    s/\@minus(?:\{\})?/-/g;
    s/\\,/,/g;

    # Now the ones that have to be replaced by special escapes
    # (which will be turned back into text by unmunge())
    s/&/&amp;/g;
    s/\@\{/&lbrace;/g;
    s/\@\}/&rbrace;/g;
    s/\@\@/&at;/g;
    # POD doesn't interpret E<> inside a verbatim block.
    if ($shift eq "") {
	s/</&lt;/g;
	s/>/&gt;/g;
    } else {
	s/</&LT;/g;
	s/>/&GT;/g;
    }

    # Single line command handlers.
    /^\@set\s+([a-zA-Z0-9_-]+)\s*(.*)$/ and $defs{$1} = $2, next;
    /^\@clear\s+([a-zA-Z0-9_-]+)/ and delete $defs{$1}, next;

    /^\@section\s+(.+)$/ and $_ = "\n=head2 $1\n";
    /^\@subsection\s+(.+)$/ and $_ = "\n=head3 $1\n";

    # Block command handlers:
    /^\@itemize\s+(\@[a-z]+|\*|-)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$_ = "\n=over 4\n";
	$endw = "itemize";
    };

    /^\@enumerate(?:\s+([A-Z0-9]+))?/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	if (defined $1) {
	    $ic = $1 . ".";
	} else {
	    $ic = "1.";
	}
	$_ = "\n=over 4\n";
	$endw = "enumerate";
    };

    /^\@table\s+(\@[a-z]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$ic =~ s/\@(?:samp|strong|key|gcctabopt|env)/B/;
	$ic =~ s/\@(?:code|kbd)/C/;
	$ic =~ s/\@(?:dfn|var|emph|cite|i)/I/;
	$ic =~ s/\@(?:file)/F/;
	$_ = "\n=over 4\n";
	$endw = "table";
    };

    /^\@((?:small)?example)/ and do {
	push @endwstack, $endw;
	$endw = $1;
	$shift = "\t";
	$_ = "";	# need a paragraph break
    };

    /^\@itemx?\s*(.+)?$/ and do {
	if (defined $1) {
	    # Entity escapes prevent munging by the <> processing below.
	    $_ = "\n=item $ic\&LT;$1\&GT;\n";
	} else {
	    $_ = "\n=item $ic\n";
	    $ic =~ y/A-Ya-y1-8/B-Zb-z2-9/;
	}
    };

    $section .= $shift.$_."\n";
}

die "No filename or title\n" unless defined $fn && defined $tl;

$sects{NAME} = "$fn \- $tl\n";
$sects{FOOTNOTES} .= "=back\n" if exists $sects{FOOTNOTES};

for $sect (qw(NAME SYNOPSIS DESCRIPTION OPTIONS ENVIRONMENT FILES
	      BUGS NOTES FOOTNOTES SEEALSO AUTHOR COPYRIGHT)) {
    if(exists $sects{$sect}) {
	$head = $sect;
	$head =~ s/SEEALSO/SEE ALSO/;
	print "=head1 $head\n\n";
	print scalar unmunge ($sects{$sect});
	print "\n";
    }
}

sub usage
{
    die "usage: $0 [-D toggle...] [infile [outfile]]\n";
}

sub postprocess
{
    local $_ = $_[0];

    # @value{foo} is replaced by whatever 'foo' is defined as.
    s/\@value\{([a-zA-Z0-9_-]+)\}/$defs{$1}/g;

    # Formatting commands.
    # Temporary escape for @r.
    s/\@r\{([^\}]*)\}/R<$1>/g;
    s/\@(?:dfn|var|emph|cite|i)\{([^\}]*)\}/I<$1>/g;
    s/\@(?:code|kbd)\{([^\}]*)\}/C<$1>/g;
    s/\@(?:gccoptlist|samp|strong|key|option|env|command|b)\{([^\}]*)\}/B<$1>/g;
    s/\@sc\{([^\}]*)\}/\U$1/g;
    s/\@file\{([^\}]*)\}/F<$1>/g;
    s/\@w\{([^\}]*)\}/S<$1>/g;
    s/\@(?:dmn|math)\{([^\}]*)\}/$1/g;

    # Cross references are thrown away, as are @noindent and @refill.
    # (@noindent is impossible in .pod, and @refill is unnecessary.)
    # @* is also impossible in .pod; we discard it and any newline that
    # follows it.  Similarly, our macro @gol must be discarded.

    s/\(?\@xref\{(?:[^\}]*)\}(?:[^.<]|(?:<[^<>]*>))*\.\)?//g;
    s/\s+\(\@pxref\{(?:[^\}]*)\}\)//g;
    s/;\s+\@pxref\{(?:[^\}]*)\}//g;
    s/\@noindent\s*//g;
    s/\@refill//g;
    s/\@gol//g;
    s/\@\*\s*\n?//g;

    # @uref can take one, two, or three arguments, with different
    # semantics each time.  @url and @email are just like @uref with
    # one argument, for our purposes.
    s/\@(?:uref|url|email)\{([^\},]*)\}/&lt;B<$1>&gt;/g;
    s/\@uref\{([^\},]*),([^\},]*)\}/$2 (C<$1>)/g;
    s/\@uref\{([^\},]*),([^\},]*),([^\},]*)\}/$3/g;

    # Turn B<blah I<blah> blah> into B<blah> I<blah> B<blah> to
    # match Texinfo semantics of @emph inside @samp.  Also handle @r
    # inside bold.
    s/&LT;/</g;
    s/&GT;/>/g;
    1 while s/B<((?:[^<>]|I<[^<>]*>)*)R<([^>]*)>/B<$1>${2}B</g;
    1 while (s/B<([^<>]*)I<([^>]+)>/B<$1>I<$2>B</g);
    1 while (s/I<([^<>]*)B<([^>]+)>/I<$1>B<$2>I</g);
    s/[BI]<>//g;
    s/([BI])<(\s+)([^>]+)>/$2$1<$3>/g;
    s/([BI])<([^>]+?)(\s+)>/$1<$2>$3/g;

    # Extract footnotes.  This has to be done after all other
    # processing because otherwise the regexp will choke on formatting
    # inside @footnote.
    while (/\@footnote/g) {
	s/\@footnote\{([^\}]+)\}/[$fnno]/;
	add_footnote($1, $fnno);
	$fnno++;
    }

    return $_;
}

sub unmunge
{
    # Replace escaped symbols with their equivalents.
    local $_ = $_[0];

    s/&lt;/E<lt>/g;
    s/&gt;/E<gt>/g;
    s/&lbrace;/\{/g;
    s/&rbrace;/\}/g;
    s/&at;/\@/g;
    s/&amp;/&/g;
    return $_;
}

sub add_footnote
{
    unless (exists $sects{FOOTNOTES}) {
	$sects{FOOTNOTES} = "\n=over 4\n\n";
    }

    $sects{FOOTNOTES} .= "=item $fnno.\n\n"; $fnno++;
    $sects{FOOTNOTES} .= $_[0];
    $sects{FOOTNOTES} .= "\n\n";
}
    
