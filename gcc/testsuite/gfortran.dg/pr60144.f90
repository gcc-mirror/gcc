! { dg-do compile }
!
! fortran PR/60144
! Contributed by Sergio Losilla
!
program ifelif
    if a=b                 ! { dg-error "Missing ... in IF-expression" }
    if (a=b                ! { dg-error "Missing ... in statement at or before" }
    if (a=b then           ! { dg-error "Missing ... in statement at or before" }
    if ((a=b)              ! { dg-error "Expected a right parenthesis in expression" }
    if ((a==b              ! { dg-error "Expected a right parenthesis in expression" }
    if ((a==b)             ! { dg-error "Missing ... in statement at or before" }
    if ((a==b) then        ! { dg-error "Missing ... in statement at or before" }
    if (a=b))              ! { dg-error "Missing ... in statement at or before" }
    if .TRUE.)             ! { dg-error "Missing ... in IF-expression" }
    if (.TRUE.)            ! { dg-error "Syntax error in IF-clause after" }
    if (.TRUE.) the        ! { dg-error "Syntax error in IF-clause after" }
    if ((.TRUE.)           ! { dg-error "Missing ... in statement at or before" }
    else if .FALSE.)       ! { dg-error "Missing ... in ELSE IF expression" }
    else if (.FALSE.       ! { dg-error "Missing ... in ELSE IF expression" }
    else if (.FALSE.)      ! { dg-error "Missing THEN in ELSE IF statement" }
    else if (.FALSE.) the  ! { dg-error "doesn't match IF label" }
    else (.true.)          ! { dg-error "Invalid character.s. in ELSE statement after" }
    else a=1               ! { dg-error "Invalid character.s. in ELSE statement after" }
    if a=b                 ! { dg-error "Missing ... in IF-expression" }
!    end if
end program
