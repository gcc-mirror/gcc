! { dg-do run }
! PR34556 Rejects valid with bogus error message: parameter initalization
! Found using the Fortran Company Fortran 90 Test Suite (Lite),
! Version 1.4
! Test case modified by Jerry DeLisle  <jvdelisle@gcc.gnu.org to
! show correct results.
module splitprms
      integer, parameter  :: nplam = 3 ! # of plans to expand TABs
      integer, parameter  :: linem = 132 ! max. line length
      integer, parameter  :: ncntm = 39 ! max. # cont. lines
      integer, parameter, dimension (linem, nplam) :: nxttab =  &
      reshape ([[(6, i= 1, 2*linem) ], [(i, i= 1,linem)],    &
                max ([(i, i= 1,linem)], [(10*i, i= 1,linem)])],      &
               [linem, nplam ])
end module splitprms

program test
  use splitprms
  if (nxttab(1, 1) .ne. 6) call abort
  if (nxttab(1, nplam) .ne. 1) call abort
  if (nxttab(linem, 1) .ne. 6) call abort
  if (nxttab(linem, nplam) .ne. 132) call abort
end program test
! { dg-final { cleanup-modules "splitprms" } }