/* Test for format extensions beyond the C standard and X/Open standard.
   Test for strftime formats.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-y2k" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  /* GCC accepts the "-", "_" and "0" flags to control padding on numeric
     formats.  It also accepts width on these formats.
  */
  /* Basic tests of parts on their own.  */
  strftime (s, m, "%5C%-C%_C%0C", tp);
  /* Correct usages.  */
  strftime (s, m, "%-5C%_5C%05C%-5d%_5d%05d%-5e%_5e%05e%-5G%_5G%05G", tp);
  strftime (s, m, "%-5H%_5H%05H%-5I%_5I%05I%-5j%_5j%05j%-5m%_5m%05m", tp);
  strftime (s, m, "%-5M%_5M%05M%-5S%_5S%05S%-5u%_5u%05u%-5U%_5U%05U", tp);
  strftime (s, m, "%-5V%_5V%05V%-5w%_5w%05w%-5W%_5W%05W%-5Y%_5Y%05Y", tp);
  /* Correct usages with GNU extension conversion characters.  */
  strftime (s, m, "%-5k%_5k%05k%-5l%_5l%05l%-20s%_20s%020s", tp);
  /* Correct usages with Y2K problems.  */
  strftime (s, m, "%-5g%_5g%05g%-5y%_5y%05y", tp); /* { dg-warning "only last 2" "2-digit year" } */
  /* Incorrect usages.  */
  strftime (s, m, "%5a", tp); /* { dg-warning "width" "bad %a" } */
  strftime (s, m, "%-a", tp); /* { dg-warning "flag" "bad %a" } */
  strftime (s, m, "%_a", tp); /* { dg-warning "flag" "bad %a" } */
  strftime (s, m, "%0a", tp); /* { dg-warning "flag" "bad %a" } */
  strftime (s, m, "%5A", tp); /* { dg-warning "width" "bad %A" } */
  strftime (s, m, "%-A", tp); /* { dg-warning "flag" "bad %A" } */
  strftime (s, m, "%_A", tp); /* { dg-warning "flag" "bad %A" } */
  strftime (s, m, "%0A", tp); /* { dg-warning "flag" "bad %A" } */
  strftime (s, m, "%5b", tp); /* { dg-warning "width" "bad %b" } */
  strftime (s, m, "%-b", tp); /* { dg-warning "flag" "bad %b" } */
  strftime (s, m, "%_b", tp); /* { dg-warning "flag" "bad %b" } */
  strftime (s, m, "%0b", tp); /* { dg-warning "flag" "bad %b" } */
  strftime (s, m, "%5B", tp); /* { dg-warning "width" "bad %B" } */
  strftime (s, m, "%-B", tp); /* { dg-warning "flag" "bad %B" } */
  strftime (s, m, "%_B", tp); /* { dg-warning "flag" "bad %B" } */
  strftime (s, m, "%0B", tp); /* { dg-warning "flag" "bad %B" } */
  strftime (s, m, "%5F", tp); /* { dg-warning "width" "bad %F" } */
  strftime (s, m, "%-F", tp); /* { dg-warning "flag" "bad %F" } */
  strftime (s, m, "%_F", tp); /* { dg-warning "flag" "bad %F" } */
  strftime (s, m, "%0F", tp); /* { dg-warning "flag" "bad %F" } */
  strftime (s, m, "%5h", tp); /* { dg-warning "width" "bad %h" } */
  strftime (s, m, "%-h", tp); /* { dg-warning "flag" "bad %h" } */
  strftime (s, m, "%_h", tp); /* { dg-warning "flag" "bad %h" } */
  strftime (s, m, "%0h", tp); /* { dg-warning "flag" "bad %h" } */
  strftime (s, m, "%5n", tp); /* { dg-warning "width" "bad %n" } */
  strftime (s, m, "%-n", tp); /* { dg-warning "flag" "bad %n" } */
  strftime (s, m, "%_n", tp); /* { dg-warning "flag" "bad %n" } */
  strftime (s, m, "%0n", tp); /* { dg-warning "flag" "bad %n" } */
  strftime (s, m, "%5p", tp); /* { dg-warning "width" "bad %p" } */
  strftime (s, m, "%-p", tp); /* { dg-warning "flag" "bad %p" } */
  strftime (s, m, "%_p", tp); /* { dg-warning "flag" "bad %p" } */
  strftime (s, m, "%0p", tp); /* { dg-warning "flag" "bad %p" } */
  strftime (s, m, "%5r", tp); /* { dg-warning "width" "bad %r" } */
  strftime (s, m, "%-r", tp); /* { dg-warning "flag" "bad %r" } */
  strftime (s, m, "%_r", tp); /* { dg-warning "flag" "bad %r" } */
  strftime (s, m, "%0r", tp); /* { dg-warning "flag" "bad %r" } */
  strftime (s, m, "%5R", tp); /* { dg-warning "width" "bad %R" } */
  strftime (s, m, "%-R", tp); /* { dg-warning "flag" "bad %R" } */
  strftime (s, m, "%_R", tp); /* { dg-warning "flag" "bad %R" } */
  strftime (s, m, "%0R", tp); /* { dg-warning "flag" "bad %R" } */
  strftime (s, m, "%5t", tp); /* { dg-warning "width" "bad %t" } */
  strftime (s, m, "%-t", tp); /* { dg-warning "flag" "bad %t" } */
  strftime (s, m, "%_t", tp); /* { dg-warning "flag" "bad %t" } */
  strftime (s, m, "%0t", tp); /* { dg-warning "flag" "bad %t" } */
  strftime (s, m, "%5T", tp); /* { dg-warning "width" "bad %T" } */
  strftime (s, m, "%-T", tp); /* { dg-warning "flag" "bad %T" } */
  strftime (s, m, "%_T", tp); /* { dg-warning "flag" "bad %T" } */
  strftime (s, m, "%0T", tp); /* { dg-warning "flag" "bad %T" } */
  strftime (s, m, "%5X", tp); /* { dg-warning "width" "bad %X" } */
  strftime (s, m, "%-X", tp); /* { dg-warning "flag" "bad %X" } */
  strftime (s, m, "%_X", tp); /* { dg-warning "flag" "bad %X" } */
  strftime (s, m, "%0X", tp); /* { dg-warning "flag" "bad %X" } */
  strftime (s, m, "%5z", tp); /* { dg-warning "width" "bad %z" } */
  strftime (s, m, "%-z", tp); /* { dg-warning "flag" "bad %z" } */
  strftime (s, m, "%_z", tp); /* { dg-warning "flag" "bad %z" } */
  strftime (s, m, "%0z", tp); /* { dg-warning "flag" "bad %z" } */
  strftime (s, m, "%5Z", tp); /* { dg-warning "width" "bad %Z" } */
  strftime (s, m, "%-Z", tp); /* { dg-warning "flag" "bad %Z" } */
  strftime (s, m, "%_Z", tp); /* { dg-warning "flag" "bad %Z" } */
  strftime (s, m, "%0Z", tp); /* { dg-warning "flag" "bad %Z" } */
  /* Incorrect usages with Y2K problems.  */
  strftime (s, m, "%5c", tp); /* { dg-warning "width" "bad %c" } */
  strftime (s, m, "%-c", tp); /* { dg-warning "flag" "bad %c" } */
  strftime (s, m, "%_c", tp); /* { dg-warning "flag" "bad %c" } */
  strftime (s, m, "%0c", tp); /* { dg-warning "flag" "bad %c" } */
  strftime (s, m, "%5D", tp); /* { dg-warning "width" "bad %D" } */
  strftime (s, m, "%-D", tp); /* { dg-warning "flag" "bad %D" } */
  strftime (s, m, "%_D", tp); /* { dg-warning "flag" "bad %D" } */
  strftime (s, m, "%0D", tp); /* { dg-warning "flag" "bad %D" } */
  strftime (s, m, "%5x", tp); /* { dg-warning "width" "bad %x" } */
  strftime (s, m, "%-x", tp); /* { dg-warning "flag" "bad %x" } */
  strftime (s, m, "%_x", tp); /* { dg-warning "flag" "bad %x" } */
  strftime (s, m, "%0x", tp); /* { dg-warning "flag" "bad %x" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 89 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 90 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 91 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 92 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 93 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 94 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 95 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 96 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 97 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 98 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 99 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 100 } */
  /* Incorrect usages with GNU extension conversion characters.  */
  strftime (s, m, "%5P", tp); /* { dg-warning "width" "bad %P" } */
  strftime (s, m, "%-P", tp); /* { dg-warning "flag" "bad %P" } */
  strftime (s, m, "%_P", tp); /* { dg-warning "flag" "bad %P" } */
  strftime (s, m, "%0P", tp); /* { dg-warning "flag" "bad %P" } */
  /* The "^" and "#" flags control the case of the output.
     ^ (uppercase) makes sense on aAbBhZ; # (change case) makes sense
     on the same and on p.
  */
  strftime (s, m, "%^a%#a%^A%#A%^b%#b%^B%#B%^h%#h%^Z%#Z%#p", tp);
  /* Bad usages.  */
  strftime (s, m, "%^C", tp); /* { dg-warning "flag" "bad %C" } */
  strftime (s, m, "%#C", tp); /* { dg-warning "flag" "bad %C" } */
  strftime (s, m, "%^d", tp); /* { dg-warning "flag" "bad %d" } */
  strftime (s, m, "%#d", tp); /* { dg-warning "flag" "bad %d" } */
  strftime (s, m, "%^e", tp); /* { dg-warning "flag" "bad %e" } */
  strftime (s, m, "%#e", tp); /* { dg-warning "flag" "bad %e" } */
  strftime (s, m, "%^F", tp); /* { dg-warning "flag" "bad %F" } */
  strftime (s, m, "%#F", tp); /* { dg-warning "flag" "bad %F" } */
  strftime (s, m, "%^G", tp); /* { dg-warning "flag" "bad %G" } */
  strftime (s, m, "%#G", tp); /* { dg-warning "flag" "bad %G" } */
  strftime (s, m, "%^H", tp); /* { dg-warning "flag" "bad %H" } */
  strftime (s, m, "%#H", tp); /* { dg-warning "flag" "bad %H" } */
  strftime (s, m, "%^I", tp); /* { dg-warning "flag" "bad %I" } */
  strftime (s, m, "%#I", tp); /* { dg-warning "flag" "bad %I" } */
  strftime (s, m, "%^j", tp); /* { dg-warning "flag" "bad %j" } */
  strftime (s, m, "%#j", tp); /* { dg-warning "flag" "bad %j" } */
  strftime (s, m, "%^m", tp); /* { dg-warning "flag" "bad %m" } */
  strftime (s, m, "%#m", tp); /* { dg-warning "flag" "bad %m" } */
  strftime (s, m, "%^M", tp); /* { dg-warning "flag" "bad %M" } */
  strftime (s, m, "%#M", tp); /* { dg-warning "flag" "bad %M" } */
  strftime (s, m, "%^n", tp); /* { dg-warning "flag" "bad %n" } */
  strftime (s, m, "%#n", tp); /* { dg-warning "flag" "bad %n" } */
  strftime (s, m, "%^p", tp); /* { dg-warning "flag" "bad %p" } */
  strftime (s, m, "%^r", tp); /* { dg-warning "flag" "bad %r" } */
  strftime (s, m, "%#r", tp); /* { dg-warning "flag" "bad %r" } */
  strftime (s, m, "%^R", tp); /* { dg-warning "flag" "bad %R" } */
  strftime (s, m, "%#R", tp); /* { dg-warning "flag" "bad %R" } */
  strftime (s, m, "%^S", tp); /* { dg-warning "flag" "bad %S" } */
  strftime (s, m, "%#S", tp); /* { dg-warning "flag" "bad %S" } */
  strftime (s, m, "%^t", tp); /* { dg-warning "flag" "bad %t" } */
  strftime (s, m, "%#t", tp); /* { dg-warning "flag" "bad %t" } */
  strftime (s, m, "%^T", tp); /* { dg-warning "flag" "bad %T" } */
  strftime (s, m, "%#T", tp); /* { dg-warning "flag" "bad %T" } */
  strftime (s, m, "%^u", tp); /* { dg-warning "flag" "bad %u" } */
  strftime (s, m, "%#u", tp); /* { dg-warning "flag" "bad %u" } */
  strftime (s, m, "%^U", tp); /* { dg-warning "flag" "bad %U" } */
  strftime (s, m, "%#U", tp); /* { dg-warning "flag" "bad %U" } */
  strftime (s, m, "%^V", tp); /* { dg-warning "flag" "bad %V" } */
  strftime (s, m, "%#V", tp); /* { dg-warning "flag" "bad %V" } */
  strftime (s, m, "%^w", tp); /* { dg-warning "flag" "bad %w" } */
  strftime (s, m, "%#w", tp); /* { dg-warning "flag" "bad %w" } */
  strftime (s, m, "%^W", tp); /* { dg-warning "flag" "bad %W" } */
  strftime (s, m, "%#W", tp); /* { dg-warning "flag" "bad %W" } */
  strftime (s, m, "%^X", tp); /* { dg-warning "flag" "bad %X" } */
  strftime (s, m, "%#X", tp); /* { dg-warning "flag" "bad %X" } */
  strftime (s, m, "%^Y", tp); /* { dg-warning "flag" "bad %Y" } */
  strftime (s, m, "%#Y", tp); /* { dg-warning "flag" "bad %Y" } */
  strftime (s, m, "%^z", tp); /* { dg-warning "flag" "bad %z" } */
  strftime (s, m, "%#z", tp); /* { dg-warning "flag" "bad %z" } */
  strftime (s, m, "%^P", tp); /* { dg-warning "flag" "bad %P" } */
  strftime (s, m, "%#P", tp); /* { dg-warning "flag" "bad %P" } */
  strftime (s, m, "%^k", tp); /* { dg-warning "flag" "bad %k" } */
  strftime (s, m, "%#k", tp); /* { dg-warning "flag" "bad %k" } */
  strftime (s, m, "%^l", tp); /* { dg-warning "flag" "bad %l" } */
  strftime (s, m, "%#l", tp); /* { dg-warning "flag" "bad %l" } */
  strftime (s, m, "%^s", tp); /* { dg-warning "flag" "bad %s" } */
  strftime (s, m, "%#s", tp); /* { dg-warning "flag" "bad %s" } */
  /* Bad usages with Y2K problems.  */
  strftime (s, m, "%^c", tp); /* { dg-warning "flag" "bad %c" } */
  strftime (s, m, "%#c", tp); /* { dg-warning "flag" "bad %c" } */
  strftime (s, m, "%^D", tp); /* { dg-warning "flag" "bad %D" } */
  strftime (s, m, "%#D", tp); /* { dg-warning "flag" "bad %D" } */
  strftime (s, m, "%^g", tp); /* { dg-warning "flag" "bad %g" } */
  strftime (s, m, "%#g", tp); /* { dg-warning "flag" "bad %g" } */
  strftime (s, m, "%^x", tp); /* { dg-warning "flag" "bad %x" } */
  strftime (s, m, "%#x", tp); /* { dg-warning "flag" "bad %x" } */
  strftime (s, m, "%^y", tp); /* { dg-warning "flag" "bad %y" } */
  strftime (s, m, "%#y", tp); /* { dg-warning "flag" "bad %y" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 182 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 183 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 184 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 185 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 186 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 187 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 188 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 189 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 190 } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 191 } */
  /* GCC also accepts the glibc format extensions %P, %k, %l, %s.  */
  strftime (s, m, "%P%k%l%s", tp);
  /* GCC also accepts the glibc extension of the "O" modifier on some
     more formats.  The cases where it is rejected altogether are
     covered in c99-strftime-1.c, except for the extension %P.
  */
  strftime (s, m, "%OC%Og%OG%Oj%OY%Oz%Ok%Ol%Os", tp); /* { dg-warning "only last 2" "2-digit year" } */
  strftime (s, m, "%OP", tp); /* { dg-warning "flag|modifier" "bad %OP" } */
  /* The "-", "_" and "0" flags are mutually exclusive.  */
  strftime (s, m, "%-_5C", tp); /* { dg-warning "flag" "bad %-_" } */
  strftime (s, m, "%-05C", tp); /* { dg-warning "flag" "bad %-0" } */
  strftime (s, m, "%_05C", tp); /* { dg-warning "flag" "bad %_0" } */
  /* The "#" and "^" flags are mutually exclusive.  */
  strftime (s, m, "%^#a", tp); /* { dg-warning "flag" "bad %^#" } */
}
