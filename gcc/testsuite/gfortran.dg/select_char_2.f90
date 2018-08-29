! { dg-do run }
! { dg-options "-O -fdump-tree-original" }

  if (foo ('E') .ne. 1) STOP 1
  if (foo ('e') .ne. 1) STOP 2
  if (foo ('f') .ne. 2) STOP 3
  if (foo ('g') .ne. 2) STOP 4
  if (foo ('h') .ne. 2) STOP 5
  if (foo ('Q') .ne. 3) STOP 6
  if (foo (' ') .ne. 4) STOP 7
  if (bar ('e') .ne. 1) STOP 8
  if (bar ('f') .ne. 3) STOP 9
contains
  function foo (c)
    character :: c
    integer :: foo
    select case (c)
      case ('E','e')
        foo = 1
      case ('f':'h  ')
        foo = 2
      case default
        foo = 3
      case ('')
        foo = 4
    end select
  end function
  function bar (c)
    character :: c
    integer :: bar
    select case (c)
      case ('ea':'ez')
        bar = 2
      case ('e')
        bar = 1
      case default
        bar = 3
      case ('fd')
        bar = 4
    end select
  end function
end

! { dg-final { scan-tree-dump-not "_gfortran_select_string" "original" } }
