! { dg-do compile }
! This test executes all code paths in gfc_arith_divide
! when executed along with it's companion test
! arith_divide_no_check.f
        implicit none
        integer i,j
        real a,b
        complex c,d
        i = 10/40
        j = 10/0! { dg-error "Division by zero at" }
        a = 10.0/40.0
        b = 10.0/0.0! { dg-error "Division by zero at" }
        c = (1.0,1.0)/(10.0,40.0) !  Not division by zero
        d = (1.0,10.)/(0.0,0.0)! { dg-error "Division by zero at" }
        end
