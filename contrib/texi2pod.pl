#! /usr/bin/perl -w

# This does trivial (and I mean _trivial_) conversion of Texinfo
# markup to Perl POD format.  It's intended to be used to extract
# something suitable for a manpage from a Texinfo document.

$in = $ARGV[0];
$out = "x";
die "usage: $0 infile outfile\n" unless defined $in && defined $out;

close STDIN;
open(IN,$in);

$output = 0;
$ignore = 0;
%sects = ();
$section = "";
@icstack = ();
@endwstack = ();
$shift = "";

while(<IN>)
{
    chomp;
    /^\@end ignore/ and $ignore = 0, next;
    next if $ignore;
    /^\@c man begin ([A-Z]+)/ and $sect = $1, $output = 1, next;
    /^\@c man end/ and do {
	$_ = $section;
	s/</&lt;/g;
	s/>/&gt;/g;

	s/\@(?:dfn|var|emph|cite)\{([^\}]*)\}/I<$1>/g;
	s/\@(?:code|kbd)\{([^\}]*)\}/C<$1>/g;
	s/\@(?:samp|strong|key)\{([^\}]*)\}/B<$1>/g;
	s/\@file\{([^\}]*)\}/F<$1>/g;
	s/\@(?:url|email)\{([^\}]*)\}/E<lt>C<$1>E<rt>/g;
	s/\@[a-z]?ref\{(?:[^\}]*)\}.?//g;
	s/\(\@p[a-z]?ref\{(?:[^\}]*)\}\).?//g;
	s/\@copyright\{\}//g;
	s/\@noindent\s*//g;
	s/\@refill//g;
	s/\@\././g;

	s/&lt;/E<lt>/g;
	s/&gt;/E<gt>/g;
	s/&LT;/</g;
	s/&GT;/>/g;

	$sects{$sect} = $_;
	$section = "";
	$output = 0;
	next;
    };
    
    /^\@(c|[a-z]+index)\b/ and next;

    /^\@setfilename\s+([^.]+)/ and $fn = $1, next;
    /^\@settitle\s+([^.]+)/ and $tl = $1, next;

    next unless $output;

    /^\@ignore/ and $ignore = 1, next;

    /^\@itemize (\@[a-z]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$ic =~ s/\@bullet/*/;
	$ic =~ s/\@minus/-/;
	$_ = "\n=over 4\n";
	$endw = "itemize";
    };

    /^\@enumerate\s+([A-Z0-9]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1 . ".";
	$_ = "\n=over 4\n";
	$endw = "enumerate";
    };

    /^\@table\s+(\@[a-z]+)/ and do {
	push @endwstack, $endw;
	push @icstack, $ic;
	$ic = $1;
	$ic =~ s/\@(?:samp|strong|key)/B/;
	$ic =~ s/\@(?:code|kbd)/C/;
	$ic =~ s/\@(?:dfn|var|emph|cite)/I/;
	$ic =~ s/\@(?:file)/F/;
	$_ = "\n=over 4\n";
	$endw = "table";
    };

    /^\@((?:small)?example)/ and do {
	push @endwstack, $endw;
	$endw = $1;
	$shift = "\t";
	next;
    };

    /^\@end\s+([a-z]+)/ and do {
	if(defined $endw)
	{
	    die "\@$endw ended by \@end $1 at line $.\n"
		unless $1 eq $endw;

	    if($endw =~ /example$/)
	    {
		$shift = "";
		$_ = "";
	    }
	    else
	    {
		$_ = "\n=back\n";
		undef $endw;
		$ic = pop @icstack;
	    }
	    $endw = pop @endwstack;
	}
    };

    /^\@itemx?\s*(.+)?$/ and do {
	if(defined $1)
	{
	    $_ = "=item $ic\&LT;$1\&GT;\n";
	}
	else
	{
	    $_ = "=item $ic\n";
	    $ic =~ y/A-Ya-y1-8/B-Zb-z2-9/;
	}
    };
	
    $section .= $shift.$_."\n";
}

$sects{NAME} = "$fn \- $tl\n";

for $sect (qw(NAME SYNOPSIS DESCRIPTION OPTIONS ENVIRONMENT FILES
	      BUGS NOTES SEEALSO AUTHOR COPYRIGHT))
{
    if(exists $sects{$sect})
    {
	$head = $sect;
	$head =~ s/SEEALSO/SEE ALSO/;
	print "=head1 $head\n\n";
	print $sects{$sect};
	print "\n";
    }
}
