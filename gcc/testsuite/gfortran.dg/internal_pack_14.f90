! { dg-do run }
!
! PR 55072: [4.6/4.7/4.8 Regression] Missing internal_pack leads to wrong code with derived type
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

program GiBUU_neutrino_bug

  Type particle
    integer :: ID
  End Type

  type(particle), dimension(1:2,1:2) :: OutPart

  OutPart(1,:)%ID = 1
  OutPart(2,:)%ID = 2

  call s1(OutPart(1,:))

contains

  subroutine s1(j)
    type(particle) :: j(:)
    print *,j(:)%ID
    call s2(j)
  end subroutine

  subroutine s2(k)
    type(particle) :: k(1:2)
    print *,k(:)%ID
    if (any (k(1:2)%ID /= [1, 1])) call abort()
  end subroutine

end
