! { dg-do run }
! Test the fix for PR127146, where the structure constructors caused errors as
! shown below.
!
! Contributed by Amir Shamoradi  <a.shahmoradi@gmail.com>
!
module mod_pdt
    implicit none
    integer, parameter :: IK = kind(1)
    integer, parameter :: IK8 = kind(1_8)
    type :: pdt_parent(IKP)
        integer, kind :: IKP
        integer(IKP) :: ivalp
    end type
    type, extends(pdt_parent) :: pdt_child(IKC)
        integer, kind :: IKC
        integer(IKC) :: ivalc
    end type
end module mod_pdt

    use mod_pdt
    implicit none
    type(pdt_child(IKP = IK, IKC = IK)) :: child1 = pdt_child(IKP = IK, IKC = IK)(ivalp = 1, ivalc = 2)
    !!  double free or corruption (fasttop)

    type(pdt_child(IKP = IK, IKC = IK8)) :: child2

    child2 = pdt_child(IKP = IK, IKC = IK8)(ivalp = 3, ivalc = 4)
    !!  Error: Component 'ivalc' is initialized twice in the structure constructor at (1)
    if ((child1%ivalp /= 1) .or. (child1%ivalc /= 2)) stop 1
    if ((child2%ivalp /= 3) .or. (child2%ivalc /= 4) .or. (kind (child2%ivalc) /= kind (1_8))) stop 2
end
