! { dg-do compile }
! Test cases where no blank is required after RETURN
subroutine sub(*)
return(1)
return1 ! { dg-error "" }
end subroutine
