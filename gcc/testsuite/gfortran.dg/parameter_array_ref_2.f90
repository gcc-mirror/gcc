! { dg-do compile }
! Test the fix for the problems in PR41044
!
! Contributed by <ros@rzg.mpg.de>
! Reduced by Joos VandeVondele <jv244@cam.ac.uk>
!
  Subroutine PS_INIT (bkgd, punit, pform, psize, rot90, bbox, clip, eps,  &
                        caller)
    type psfd                          ! paper size and frame defaults
      character(3)                     :: n
      real                             :: p(2)
      real                             :: f(4)
    end type psfd
    character(4)                       :: fn, orich, pfmt
    type(psfd), parameter              :: pfd(0:11)=(/  &
         psfd('   ',(/   0.0,   0.0/),(/200.,120.,800.,560./)), &    ! A0_L
         psfd('A0 ',(/ 840.9,1189.2/),(/140., 84.,560.,400./)), &    ! A0_P
         psfd('A1 ',(/ 594.6, 840.9/),(/100., 60.,400.,280./)), &    ! A1_P
         psfd('A2 ',(/ 420.4, 594.6/),(/ 70., 42.,280.,200./)), &    ! A2_P
         psfd('A3 ',(/ 297.3, 420.4/),(/ 50., 30.,200.,140./)), &    ! A3_P
         psfd('A4 ',(/ 210.2, 297.3/),(/ 35., 21.,140.,100./)), &    ! A4_P
         psfd('A5 ',(/ 148.7, 210.2/),(/ 25., 15.,100., 70./)), &    ! A5_P
         psfd('A6 ',(/ 105.1, 148.7/),(/ 18., 11., 70., 50./)), &    ! A6_P
         psfd('   ',(/   0.0,   0.0/),(/ 50., 30.,200.,140./)), &    ! Letter_L
         psfd('LET',(/ 215.9, 279.4/),(/ 35., 21.,140.,100./)), &    ! Letter_P
         psfd('   ',(/   0.0,   0.0/),(/ 50., 30.,200.,140./)), &    ! Legal_L
         psfd('LEG',(/ 215.9, 355.6/),(/ 35., 21.,140.,100./))/)     ! Legal_P
    if (len_trim(pfmt) > 0) then       ! set paper format
      idx=sum(maxloc(index(pfd%n,pfmt(1:3))))-1
    end if
  end subroutine PS_INIT

! This, additional problem, was posted as comment #8 by Tobias Burnus <burnus@gcc.gnu.org>
  type t
    integer :: i
  end type t
  type(t), parameter :: a(1) = t(4) ! [t(4)] worked OK
  real(a(1)%i) :: b
end
