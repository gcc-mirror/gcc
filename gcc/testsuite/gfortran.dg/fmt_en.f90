! { dg-do run }
! PR60128 Invalid outputs with EN descriptors
! Test case provided by Walt Brainerd.
program pr60128
implicit none
    integer :: n_tst = 0, n_cnt = 0

! Original test.
    call checkfmt("(en15.2)", -.44444,    "    -444.44E-03")

! Test for the bug in comment 6.
    call checkfmt("(en15.0)", 1.0,        "         1.E+00")
    call checkfmt("(en15.0)", 1.00000012, "         1.E+00")
    call checkfmt("(en15.0)", 0.99999994, "         1.E+00")
    call checkfmt("(en15.0)", 10.0,       "        10.E+00")
    call checkfmt("(en15.0)", 10.0000010, "        10.E+00")
    call checkfmt("(en15.0)", 9.99999905, "        10.E+00")
    call checkfmt("(en15.0)", 100.0,      "       100.E+00")
    call checkfmt("(en15.0)", 100.000008, "       100.E+00")
    call checkfmt("(en15.0)", 99.9999924, "       100.E+00")
    call checkfmt("(en15.0)", 1000.0,     "         1.E+03")
    call checkfmt("(en15.0)", 1000.00006, "         1.E+03")
    call checkfmt("(en15.0)", 999.999939, "         1.E+03")
    call checkfmt("(en15.0)", 9.5,        "        10.E+00")
    call checkfmt("(en15.0)", 9.50000095, "        10.E+00")
    call checkfmt("(en15.0)", 9.49999905, "         9.E+00")
    call checkfmt("(en15.0)", 99.5,       "       100.E+00")
    call checkfmt("(en15.0)", 99.5000076, "       100.E+00")
    call checkfmt("(en15.0)", 99.4999924, "        99.E+00")
    call checkfmt("(en15.0)", 999.5,      "         1.E+03")
    call checkfmt("(en15.0)", 999.500061, "         1.E+03")
    call checkfmt("(en15.0)", 999.499939, "       999.E+00")
    call checkfmt("(en15.0)", 9500.0,     "        10.E+03")
    call checkfmt("(en15.0)", 9500.00098, "        10.E+03")
    call checkfmt("(en15.0)", 9499.99902, "         9.E+03")
    call checkfmt("(en15.1)", 9950.0,     "       10.0E+03")
    call checkfmt("(en15.2)", 9995.0,     "      10.00E+03")
    call checkfmt("(en15.3)", 9999.5,     "     10.000E+03")
    call checkfmt("(en15.1)", 9.5,        "        9.5E+00")
    call checkfmt("(en15.1)", 9.50000095, "        9.5E+00")
    call checkfmt("(en15.1)", 9.49999905, "        9.5E+00")
    call checkfmt("(en15.1)", 0.099951,   "      100.0E-03")
    call checkfmt("(en15.1)", 0.009951,   "       10.0E-03")
    call checkfmt("(en15.1)", 0.000999951,"        1.0E-03")

    call checkfmt("(en15.0)", -1.0,        "        -1.E+00")
    call checkfmt("(en15.0)", -1.00000012, "        -1.E+00")
    call checkfmt("(en15.0)", -0.99999994, "        -1.E+00")
    call checkfmt("(en15.0)", -10.0,       "       -10.E+00")
    call checkfmt("(en15.0)", -10.0000010, "       -10.E+00")
    call checkfmt("(en15.0)", -9.99999905, "       -10.E+00")
    call checkfmt("(en15.0)", -100.0,      "      -100.E+00")
    call checkfmt("(en15.0)", -100.000008, "      -100.E+00")
    call checkfmt("(en15.0)", -99.9999924, "      -100.E+00")
    call checkfmt("(en15.0)", -1000.0,     "        -1.E+03")
    call checkfmt("(en15.0)", -1000.00006, "        -1.E+03")
    call checkfmt("(en15.0)", -999.999939, "        -1.E+03")
    call checkfmt("(en15.0)", -9.5,        "       -10.E+00")
    call checkfmt("(en15.0)", -9.50000095, "       -10.E+00")
    call checkfmt("(en15.0)", -9.49999905, "        -9.E+00")
    call checkfmt("(en15.0)", -99.5,       "      -100.E+00")
    call checkfmt("(en15.0)", -99.5000076, "      -100.E+00")
    call checkfmt("(en15.0)", -99.4999924, "       -99.E+00")
    call checkfmt("(en15.0)", -999.5,      "        -1.E+03")
    call checkfmt("(en15.0)", -999.500061, "        -1.E+03")
    call checkfmt("(en15.0)", -999.499939, "      -999.E+00")
    call checkfmt("(en15.0)", -9500.0,     "       -10.E+03")
    call checkfmt("(en15.0)", -9500.00098, "       -10.E+03")
    call checkfmt("(en15.0)", -9499.99902, "        -9.E+03")
    call checkfmt("(en15.1)", -9950.0,     "      -10.0E+03")
    call checkfmt("(en15.2)", -9995.0,     "     -10.00E+03")
    call checkfmt("(en15.3)", -9999.5,     "    -10.000E+03")
    call checkfmt("(en15.1)", -9.5,        "       -9.5E+00")
    call checkfmt("(en15.1)", -9.50000095, "       -9.5E+00")
    call checkfmt("(en15.1)", -9.49999905, "       -9.5E+00")
    call checkfmt("(en15.1)", -0.099951,   "     -100.0E-03")
    call checkfmt("(en15.1)", -0.009951,   "      -10.0E-03")
    call checkfmt("(en15.1)", -0.000999951,"       -1.0E-03")

    call checkfmt("(en15.1)", 987350.,     "      987.4E+03")
    call checkfmt("(en15.2)", 98735.,      "      98.74E+03")
    call checkfmt("(en15.3)", 9873.5,      "      9.874E+03")
    call checkfmt("(en15.1)", 987650.,     "      987.6E+03")
    call checkfmt("(en15.2)", 98765.,      "      98.76E+03")
    call checkfmt("(en15.3)", 9876.5,      "      9.876E+03")
    call checkfmt("(en15.1)", 3.125E-02,   "       31.2E-03")
    call checkfmt("(en15.1)", 9.375E-02,   "       93.8E-03")
    call checkfmt("(en15.2)", 1.5625E-02,  "      15.62E-03")
    call checkfmt("(en15.2)", 4.6875E-02,  "      46.88E-03")
    call checkfmt("(en15.3)", 7.8125E-03,  "      7.812E-03")
    call checkfmt("(en15.3)", 2.34375E-02, "     23.438E-03")
    call checkfmt("(en15.3)", 9.765625E-04,"    976.562E-06")
    call checkfmt("(en15.6)", 2.9296875E-03,"   2.929688E-03")

    call checkfmt("(en15.1)", -987350.,     "     -987.4E+03")
    call checkfmt("(en15.2)", -98735.,      "     -98.74E+03")
    call checkfmt("(en15.3)", -9873.5,      "     -9.874E+03")
    call checkfmt("(en15.1)", -987650.,     "     -987.6E+03")
    call checkfmt("(en15.2)", -98765.,      "     -98.76E+03")
    call checkfmt("(en15.3)", -9876.5,      "     -9.876E+03")
    call checkfmt("(en15.1)", -3.125E-02,   "      -31.2E-03")
    call checkfmt("(en15.1)", -9.375E-02,   "      -93.8E-03")
    call checkfmt("(en15.2)", -1.5625E-02,  "     -15.62E-03")
    call checkfmt("(en15.2)", -4.6875E-02,  "     -46.88E-03")
    call checkfmt("(en15.3)", -7.8125E-03,  "     -7.812E-03")
    call checkfmt("(en15.3)", -2.34375E-02, "    -23.438E-03")
    call checkfmt("(en15.3)", -9.765625E-04,"   -976.562E-06")
    call checkfmt("(en15.6)", -2.9296875E-03,"  -2.929688E-03")

    !print *, n_tst, n_cnt
    if (n_cnt /= 0) call abort

contains
    subroutine checkfmt(fmt, x, cmp)
        use ISO_FORTRAN_ENV
        implicit none
        integer, parameter :: j(size(real_kinds)+4)=[REAL_KINDS, [4, 4, 4, 4]]
        integer :: i
        character(len=*), intent(in) :: fmt
        real, intent(in) :: x
        character(len=*), intent(in) :: cmp
        character(len=20) :: s
        do i=1,size(real_kinds)
          if (i == 1) then
            write(s, fmt) real(x,kind=j(1))
          else if (i == 2) then
            write(s, fmt) real(x,kind=j(2))
          else if (i == 3) then
            write(s, fmt) real(x,kind=j(3))
          else if (i == 4) then
            write(s, fmt) real(x,kind=j(4))
          end if
          n_tst = n_tst + 1
          if (s /= cmp) then
             print "(a,1x,a,' expected: ',1x,a)", fmt, s, cmp
             n_cnt = n_cnt + 1
           end if
        end do
        
    end subroutine
end program
