C When run through the C preprocessor, the indentation of the
C CONTINUE line must not be mangled.
      subroutine aap(a, n)
      dimension a(n)
      do 10 i = 1, n
         a(i) = i
 10   continue
      print *, a(1)
      end
