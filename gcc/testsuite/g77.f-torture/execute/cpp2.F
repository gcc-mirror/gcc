C The preprocessor must not mangle Hollerith constants
C which contain apostrophes.
      integer i, j
      data i /4hbla'/
      data j /"bla'"/
      if (i .ne. j) call abort
      end
