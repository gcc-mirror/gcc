! { dg-do run }
! Checks for corrects warnings if branching to then end of a
! construct at various nesting levels
  subroutine check_if(i)
    goto 10
    if (i > 0) goto 40
    if (i < 0) then
       goto 40
10  end if
    if (i == 0) then
       i = i+1
       goto 20  ! { dg-warning "jumps to END of construct" }
       goto 40
20  end if   ! { dg-warning "jumps to END of construct" }
    if (i == 1) then
       i = i+1
       if (i == 2) then
          goto 30 ! { dg-warning "jumps to END of construct" }
       end if
       goto 40
30  end if    ! { dg-warning "jumps to END of construct" }
    return
40  i = -1
  end subroutine check_if
  
  subroutine check_select(i)
    goto 10
    select case (i)
    case default
       goto 999
10  end select
    select case (i)
    case (2)
       i = 1
       goto 20  ! { dg-warning "jumps to END of construct" }
       goto 999
    case default
       goto 999
20  end select   ! { dg-warning "jumps to END of construct" }
    j = i
    select case (j)
    case default
       select case (i)
       case (1)
          i = 2
          goto 30  ! { dg-warning "jumps to END of construct" }
       end select
       goto 999
30  end select    ! { dg-warning "jumps to END of construct" }
    return    
999 i = -1
  end subroutine check_select

  i = 0
  call check_if (i)
  if (i /= 2) call abort ()
  call check_select (i)
  if (i /= 2) call abort ()
end
