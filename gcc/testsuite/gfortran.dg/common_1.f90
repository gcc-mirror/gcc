! { dg-do compile }
! tests various allowed variants of the common statement
! inspired by PR 18869

! blank common block
      common x
      common y, z
      common // xx

! one named common block on a line
      common /a/ e
      
! appending to a common block
      common /a/ g

! several named common blocks on a line
      common /foo/ a, /bar/ b      ! note 'a' is also the name of the 
                                   ! above common block
      common /baz/ c /foobar/ d, /bazbar/ f

      end
