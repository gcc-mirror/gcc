c { dg-do run }
C The preprocessor must not mangle Hollerith constants
C which contain apostrophes.
      integer i
      character(4) j
      data i /4hbla'/
      write (j, '(4a)') i
      if (j .ne. "bla'") STOP 1
      end

      ! { dg-warning "Hollerith constant" "const" { target *-*-* } 6 }
      ! { dg-warning "Conversion" "conversion" { target *-*-* } 6 }
