! { dg-do compile }
      character*2 a
      character*4 b
      parameter (a="12")
      parameter (b = a(1:2))
      end
