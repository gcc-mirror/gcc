! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for pr122696, which used to ICE on the associate statement. The
! generic selector tensor_t () should resolve to tensor ().
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(0.)
  end type

  interface tensor_t
    module function tensor()
      implicit none
      type(tensor_t) tensor
    end function
  end interface

end module

  use tensor_m
  implicit none
  associate(t => tensor_t())
  end associate
end
! { dg-final { scan-tree-dump-times "t = tensor \\(\\)" 1 "original" } }
