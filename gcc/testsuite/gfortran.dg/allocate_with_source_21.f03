! { dg-do compile }

! Check fix for pr71936.
! Contributed by Gerhard Steinmetz

program p
  type t
  end type

  call test2()
  call test4()
  call test1()
  call test3()
contains
  function f_p()
    class(t), pointer :: f_p(:)
    nullify(f_p)
  end

  function f_a()
    class(t), allocatable :: f_a(:)
  end

  subroutine test1()
    class(t), allocatable :: x(:)
    allocate (x, mold=f_a())
    deallocate (x)
    allocate (x, source=f_a())
  end subroutine

  subroutine test2()
    class(t), pointer :: x(:)
    allocate (x, mold=f_p())
    deallocate (x)
    allocate (x, source=f_p())
  end

  subroutine test3()
    class(t), pointer :: x(:)
    allocate (x, mold=f_a())
    deallocate (x)
    allocate (x, source=f_a())
  end

  subroutine test4()
    class(t), allocatable :: x(:)
    allocate (x, mold=f_p())
    deallocate (x)
    allocate (x, source=f_p())
  end subroutine
end

