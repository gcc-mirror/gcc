! Program to test contained functions calling their siblings.
! This is tricky because we don't find the declaration for the sibling
! function until after the caller has been parsed.
program contained_3
  call test
contains
  subroutine test
    if (sub(3) .ne. 6) call abort
  end subroutine
  integer function sub(i)
    integer i
    if (i .gt. 1) then
      sub = sub2(i) * i
    else
      sub = 1
    end if
  end function
  integer function sub2(i)
    integer i
    sub2 = sub(i - 1)
  end function
end program
