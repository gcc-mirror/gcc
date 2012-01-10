! { dg-do compile }
!
! PR fortran/51605
!

contains
  subroutine foo
    BLOCK_NAME: block
    end block BLOCK_NAME
  end subroutine foo

  subroutine BLOCK_NAME()
  end subroutine BLOCK_NAME

  subroutine bar()
  end subroutine bar
end

subroutine test()
contains
  subroutine BLOCK_NAME()
  end subroutine BLOCK_NAME

  subroutine foobar()
  end subroutine foobar

  subroutine foo
    BLOCK_NAME: block
    end block BLOCK_NAME
  end subroutine foo

  subroutine bar()
  end subroutine bar
end
