C Test compiler flags: -fugly-assumed
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do compile }
C { dg-options "-fugly-assumed" }
      function f(i)
      integer i(1)
      f = i(1)+i(2)
      end
