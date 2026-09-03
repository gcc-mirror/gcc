 ! { dg-do run }
!
! Test the fix for pr104750, in which PDT extension outside the module of the
! parent type led to the errors:
!      52 |         integer(IKP) :: ivalc1
!         |                1
!   Error: Parameter ‘ikp’ at (1) has not been declared or is
!   a variable, which does not reduce to a constant expression.
!   pdt_93_exmodext.f90:47:13:
!
!      54 |         real(RKP) :: rvalc1
!         |             1
! Produces the same error message, followed by the fallout errors.
!
! Contributed by Amir Shahmoradi <shahmoradi@lanl.gov>
!
module mod_grandparent

    integer, parameter :: IK = kind(1)
    integer, parameter :: LK = kind(.true.)
    integer, parameter :: RKS = kind(1.e0)
    integer, parameter :: RKD = kind(1.d0)

    type, abstract :: pdt_grandparent(LKGP)
        integer, kind :: LKGP = kind(.true.)
        logical(LKGP) :: lvalgp
    end type

end module

module mod_parent

    use mod_grandparent, only: pdt_grandparent
    use mod_grandparent, only: IK, LK, RKS, RKD

    type, extends(pdt_grandparent) :: pdt_parent(IKP, RKP)
        integer, kind :: IKP = IK
        integer, kind :: RKP
        integer(IKP) :: ivalp
        real(RKP) :: rvalp
    end type

end module

module mod_child

    use mod_parent, only: pdt_parent
    use mod_parent, only: IK, LK, RKS, RKD

    type, extends(pdt_parent) :: pdt_child(IKC, RKC)
        integer, kind :: IKC, RKC
        integer(IKP) :: ivalc1
        integer(IKC) :: ivalc2
        real(RKP) :: rvalc1
        real(RKC) :: rvalc2
    end type

    interface pdt_child
        module procedure :: pdt_child_getter
    end interface

contains

    pure function pdt_child_getter(lvalgp, ivalp, rvalp, ivalc1, ivalc2, rvalc1, rvalc2) result(child)
        type(pdt_child(RKP = RKS, IKC = IK, RKC = RKD)) :: child
        logical(LK), intent(in), optional :: lvalgp
        integer(IK), intent(in), optional :: ivalp
        real(RKS), intent(in), optional :: rvalp
        integer(IK), intent(in), optional :: ivalc1
        integer(IK), intent(in), optional :: ivalc2
        real(RKS), intent(in), optional :: rvalc1
        real(RKD), intent(in), optional :: rvalc2
        if (present(lvalgp)) child%lvalgp = lvalgp
        if (present(ivalp)) child%ivalp = ivalp
        if (present(rvalp)) child%rvalp = rvalp
        if (present(ivalc1)) child%ivalc1 = ivalc1
        if (present(ivalc2)) child%ivalc2 = ivalc2
        if (present(rvalc1)) child%rvalc1 = rvalc1
        if (present(rvalc2)) child%rvalc2 = rvalc2
    end function

end module

module mod_grandchild

    use mod_child, only: pdt_child
    use mod_child, only: IK, LK, RKS, RKD

    type, extends(pdt_child) :: pdt_grandchild(IKGC, RKGC)
        integer, kind :: IKGC, RKGC
        integer(IKP) :: ivalgc1
        integer(IKC) :: ivalgc2
        integer(IKGC) :: ivalgc3
        real(RKP) :: rvalgc1
        real(RKC) :: rvalgc2
        real(RKGC) :: rvalgc3
    end type

end module

program test_pdt_extension

    use mod_child, only: pdt_child
    use mod_grandchild, only: pdt_grandchild
    use mod_grandchild, only: IK, LK, RKS, RKD

    implicit none

    type(pdt_grandchild(RKP = RKS, IKC = IK, RKC = RKD, IKGC = IK, RKGC = RKD)) :: gc

    gc%ivalp = 1
    gc%rvalp = 1

    gc%ivalc1 = 2
    gc%ivalc2 = 2
    gc%rvalc1 = 2
    gc%rvalc2 = 2

    gc%ivalgc1 = 3
    gc%ivalgc2 = 3
    gc%ivalgc3 = 3
    gc%rvalgc1 = 3
    gc%rvalgc2 = 3
    gc%rvalgc3 = 3

    !!!!
    !!!!    Ensure components are assigned correctly.
    !!!!

    if (gc%ivalp /= gc%rvalp) stop 1

    if (gc%ivalc1 /= gc%ivalc2) stop 2
    if (gc%ivalc1 /= gc%rvalc1) stop 3
    if (gc%ivalc1 /= gc%rvalc2) stop 4

    if (gc%ivalgc1 /= gc%ivalgc2) stop 5
    if (gc%ivalgc1 /= gc%ivalgc3) stop 6
    if (gc%ivalgc1 /= gc%rvalgc1) stop 7
    if (gc%ivalgc1 /= gc%rvalgc2) stop 8
    if (gc%ivalgc1 /= gc%rvalgc3) stop 9

    block

        use mod_child, only: pdt_child

        !!!!
        !!!!    Custom Constructor: Ensure whole PDT assignment through parental access works.
        !!!!

        gc%pdt_child = pdt_child( ivalc1 = -2_IK &
                                , lvalgp = .false._LK &
                                , ivalc2 = -2_IK &
                                , rvalc1 = -2._RKS &
                                , rvalc2 = -2._RKD &
                                )

        if (gc%pdt_child%lvalgp .neqv. .false.) stop 10
        if (gc%ivalc1 /= -2) stop 11
        if (gc%ivalc2 /= -2) stop 12
        if (gc%rvalc1 /= -2) stop 13
        if (gc%rvalc2 /= -2) stop 14

    end block

end program
