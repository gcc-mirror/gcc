! { dg-do compile }
! { dg-options "-fno-range-check" }
! This test executes all code paths in gfc_arith_divide
! when executed along with it's companion test
! arith_divide.f

        implicit none
        integer i,j
        real a,b
        complex c,d
        i = 10/40
        j = 10/0! { dg-error "Division by zero at" }
        a = 10.0/40.0
        b = 10.0/0.0
        c = (1.0,1.0)/(10.0,40.0)
        d = (1.0,10.)/(0.0,0.0)
        end
