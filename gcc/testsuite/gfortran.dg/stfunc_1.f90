! { dg-do run }
! this is a problem which disappeared between 2005-01-02 and 2005-03-13
! PR 18600
      logical a, b
      a(b) = .true.
      b = .false.
      if (a(.false.)) b = .true.
      if (.not.b) call abort
      end
