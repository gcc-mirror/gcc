! { dg-do compile }

module yemdyn
   implicit none
   integer, parameter :: jpim = selected_int_kind(9)
   integer, parameter :: jprb = selected_real_kind(13,300)
   real(kind=jprb) :: elx
   real(kind=jprb), allocatable :: xkcoef(:)
   integer(kind=jpim),allocatable :: ncpln(:), npne(:)
end module yemdyn

subroutine suedyn

   use yemdyn

   implicit none

   integer(kind=jpim) :: jm, jn
   real(kind=jprb) :: zjm, zjn, zxxx

   jn=0
   do jm=0,ncpln(jn)
      zjm=real(jm,jprb) / elx
      xkcoef(npne(jn)+jm) = - zxxx*(zjm**2)**0.5_jprb
   end do

end subroutine suedyn

! { dg-final { cleanup-tree-dump "vect" } }
