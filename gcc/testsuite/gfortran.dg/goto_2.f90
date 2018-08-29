! { dg-do run }
! Checks for corrects warnings if branching to then end of a
! construct at various nesting levels
  subroutine check_if(i)
    goto 10  ! { dg-warning "Label at ... is not in the same block" }
    if (i > 0) goto 40
    if (i < 0) then
       goto 40
10  end if    ! { dg-warning "Label at ... is not in the same block" }
    if (i == 0) then
       i = i+1
       goto 20
       goto 40
20  end if
    if (i == 1) then
       i = i+1
       if (i == 2) then
          goto 30
       end if
       goto 40
30  end if
    return
40  i = -1
  end subroutine check_if
  
  subroutine check_select(i)
    goto 10  ! { dg-warning "Label at ... is not in the same block" }
    select case (i)
    case default
       goto 999
10  end select  ! { dg-warning "Label at ... is not in the same block" }
    select case (i)
    case (2)
       i = 1
       goto 20
       goto 999
    case default
       goto 999
20  end select
    j = i
    select case (j)
    case default
       select case (i)
       case (1)
          i = 2
          goto 30
       end select
       goto 999
30  end select
    return    
999 i = -1
  end subroutine check_select

  i = 0
  call check_if (i)
  if (i /= 2) STOP 1
  call check_select (i)
  if (i /= 2) STOP 2
end
