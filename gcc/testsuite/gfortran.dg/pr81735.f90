! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Contributed by Danila  <flashmozzg@gmail.com>
!
program fooprog
    implicit none
    type FooType
        integer, allocatable :: x
    end type FooType

    type(FooType), pointer :: bar

    bar => foo()

contains
    function foo() result(res)
        type(FooType), pointer :: res

        character(:), allocatable :: rt
        rt = ""
        res => null()
    end function foo
end program fooprog
! { dg-final { scan-tree-dump-times "__builtin_free" 1 "original" } }
