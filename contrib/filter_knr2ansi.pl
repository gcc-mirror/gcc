#!/usr/bin/perl
#
# Goes thourgh the input line by line to find K&R style function
# declarations, and replaces them with ANSI style declarations.
#
@blah = <>;

for ($i = 0; $i < @blah; $i++)
{
	if ($blah[$i] =~ /^([a-zA-Z_0-9]+)\s*\([^)]+\)\s*$/)
	{
		$name = $1;
		$funci = $i;
		$blah[$funci]="$name (";
		$i++;
		$lastline = $i;
		while ($lastline < @blah && $blah[$lastline] !~ /^{/)
		{
			$lastline++;
		}
		$lastline--;
		while ($i < @blah && $blah[$i] !~ /^{/)
		{
			$arg = $blah[$i];
			if ($i != $lastline)
			{
				$arg =~ s/;/,/g;
			}
			else
			{
				$arg =~ s/;//g;
			}
			$blah[$i] = "";
			$blah[$funci] = "$blah[$funci]" . "$arg";
			$i++;
		}
		$blah[$funci] = "$blah[$funci]" . ")\n";
	}
}

for ($i = 0; $i < @blah; $i++)
{
	print $blah[$i];
}

