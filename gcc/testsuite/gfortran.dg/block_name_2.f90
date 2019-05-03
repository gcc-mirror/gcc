! { dg-do compile }
! Test that various illegal combinations of block statements with
! block names yield the correct error messages.  Motivated by PR31471.
program blocks
  dimension a(5,2)

  a = 0

  ! The END statement of a labelled block needs to carry the construct
  ! name.
  d1: do i=1,10
  end do      ! { dg-error "Expected block name of .... in END DO statement" }
  end do d1

  i1: if (i > 0) then
  end if      ! { dg-error "Expected block name of .... in END IF statement" }
  end if i1

  s1: select case (i)
  end select ! { dg-error "Expected block name of .... in END SELECT statement" }
  end select s1

  w1: where (a > 0)
  end where ! { dg-error "Expected block name of .... in END WHERE statement" }
  end where w1

  f1: forall (i = 1:10)
  end forall ! { dg-error "Expected block name of .... in END FORALL statement" }
  end forall f1

  ! A construct name may not appear in the END statement, if it
  ! doesn't appear in the statement beginning the block.
  ! Likewise it may not appear in ELSE IF, ELSE, ELSEWHERE or CASE
  ! statements.
  do i=1,10
  end do d2 ! { dg-error "Syntax error in END DO statement" }
  end do

  if (i > 0) then
  else if (i ==0) then i2 ! { dg-error "Syntax error in ELSE IF statement" }
  else i2 ! { dg-error "Invalid character.s. in ELSE statement" }
  end if i2 ! { dg-error "Syntax error in END IF statement" }
  end if

  select case (i)
  case (1) s2  ! { dg-error "Syntax error in CASE specification" }
  case default s2 ! { dg-error "Syntax error in CASE specification" }
  end select s2 ! { dg-error "Syntax error in END SELECT statement" }
  end select

  where (a > 0)
  elsewhere w2  ! { dg-error "Invalid character.s. in ELSE statement" }
  end where w2 ! { dg-error "Syntax error in END WHERE statement" }
  end where

  forall (i=1:10)
  end forall f2 ! { dg-error "Syntax error in END FORALL statement" }
  end forall
  
end program blocks
