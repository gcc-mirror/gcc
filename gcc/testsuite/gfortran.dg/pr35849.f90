! { dg-do compile }
! PR35849
INTEGER, PARAMETER                 :: j = 15
INTEGER, PARAMETER, DIMENSION(10)  :: A = [(i, i = 1,10)]
INTEGER, PARAMETER, DIMENSION(10)  :: B = ISHFTC(j, A, -20) ! { dg-error "must be positive" }
INTEGER, PARAMETER, DIMENSION(10)  :: C = ISHFTC(1_1, A, j) ! { dg-error "less than or equal to BIT_SIZE" }
INTEGER, PARAMETER, DIMENSION(10)  :: D = ISHFTC(3, A, 5) ! { dg-error "Absolute value of SHIFT shall be less than or equal" }
INTEGER, PARAMETER, DIMENSION(10)  :: E = ISHFTC(3_1, A) ! { dg-error "second argument of ISHFTC exceeds BIT_SIZE of first argument" }
end
