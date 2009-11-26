#!/usr/bin/perl

$nelt = int($ARGV[0]);
$leng = int($ARGV[1]);

print "/* This file auto-generated with ./vperm.pl $nelt $leng.  */\n\n";

for ($i = 0; $i < $nelt; ++$i) { $perm[$i] = 0; }
$ncheck = 0;

for ($i = 0; $i < ($leng * $nelt) ** $nelt; ++$i)
{
  if ($i % 128 == 0)
  {
    print "}\n\n" if $ncheck > 0;
    print "void check$ncheck(void)\n{\n";
    ++$ncheck;
  }

  print "  TEST (";
  for ($j = 0; $j < $nelt; ++$j)
  {
    print $perm[$j];
    print ", " if $j < $nelt - 1;
  }
  print ")\n";

  INCR: for ($j = 0; $j < $nelt; ++$j)
  {
    last INCR if ++$perm[$j] < $leng * $nelt;
    $perm[$j] = 0;
  }
}
print "}\n\n";

print "void check(void)\n{\n";
for ($i = 0; $i < $ncheck; ++$i)
{
  print "  check$i ();\n";
}
print "}\n\n";
