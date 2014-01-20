! { dg-do run }
! PR48615 Invalid UP/DOWN rounding with E and ES descriptors
! Test case provided by Thomas Henlich.
program pr48615
    call checkfmt("(RU,F17.0)", 2.5,     "               3.")
    call checkfmt("(RU,-1P,F17.1)", 2.5, "              0.3")
    call checkfmt("(RU,E17.1)", 2.5,     "          0.3E+01")
    call checkfmt("(RU,1P,E17.0)", 2.5,  "           3.E+00")
    call checkfmt("(RU,ES17.0)", 2.5,    "           3.E+00")
    call checkfmt("(RU,EN17.0)", 2.5,    "           3.E+00")
    call checkfmt("(RU,F2.0)",      2.0,  "2.")
    call checkfmt("(RU,F6.4)",      2.0,  "2.0000")
    call checkfmt("(RU,1P,E6.0E2)", 2.0,  "2.E+00")
    call checkfmt("(RU,1P,E7.1E2)", 2.5,  "2.5E+00")
    call checkfmt("(RU,1P,E10.4E2)", 2.5,  "2.5000E+00")
    call checkfmt("(RU,1P,G6.0E2)", 2.0,  "2.E+00")
    call checkfmt("(RU,1P,G10.4E2)", 2.3456e5,  "2.3456E+05")

    call checkfmt("(RC,G10.2)", 99.5,   "  0.10E+03") ! pr59774
    call checkfmt("(RC,G10.2)", 995.,   "  0.10E+04") ! pr59774
    call checkfmt("(RC,G10.3)", 999.5,  " 0.100E+04") ! pr59774
    call checkfmt("(RC,G10.3)", 9995.,  " 0.100E+05") ! pr59774
    call checkfmt("(RU,G10.2)", .099,   "  0.10    ") ! pr59774
    call checkfmt("(RC,G10.1)", .095,   "   0.1    ") ! pr59774
    call checkfmt("(RU,G10.3)", .0999,  " 0.100    ") ! pr59774
    call checkfmt("(RC,G10.2)", .0995,  "  0.10    ") ! pr59774

    call checkfmt("(RU,G9.3)",  891.1,  " 892.")      ! pr59836
    call checkfmt("(RD,G9.3)", -891.1,  "-892.")      ! pr59836

    call checkfmt("(RU,F2.0)",     0.09,  "1.")       ! 0.
    call checkfmt("(RD,F3.0)",     -0.09,  "-1.")     ! -0.
    call checkfmt("(RU,F2.0)",     0.9,  "1.")        ! pr59836
    call checkfmt("(RC,F2.0)",     0.4,  "0.")        ! pr59836
    call checkfmt("(RC,F2.0)",     0.5,  "1.")        ! pr59836
    call checkfmt("(RC,F2.0)",     0.6,  "1.")        ! pr59836
    call checkfmt("(RD,F3.0)",     -0.9,  "-1.")      ! pr59836
    call checkfmt("(RC,F3.0)",     -0.4,  "-0.")      ! pr59836
    call checkfmt("(RC,F3.0)",     -0.5,  "-1.")      ! pr59836
    call checkfmt("(RC,F3.0)",     -0.6,  "-1.")      ! pr59836
    call checkfmt("(RU,F2.0)",      2.0,  "2.")       ! 3.
    call checkfmt("(RD,F3.0)",     -2.0,  "-2.")      ! -3.
    call checkfmt("(RU,F6.4)",      2.0,  "2.0000")   ! 2.0001
    call checkfmt("(RD,F7.4)",     -2.0,  "-2.0000")  ! -2.0001
    call checkfmt("(RU,1P,E6.0E2)", 2.0,  "2.E+00")   ! 3.E+00
    call checkfmt("(RD,1P,E7.0E2)", -2.0,  "-2.E+00") ! -3.E+00
    call checkfmt("(RU,1P,E7.1E2)", 2.5,  "2.5E+00")  ! 2.6E+00
    call checkfmt("(RD,1P,E8.1E2)", -2.5,  "-2.5E+00") ! -2.6E+00
    call checkfmt("(RU,1P,E10.4E2)", 2.5,  "2.5000E+00") ! 2.5001E+00
    call checkfmt("(RD,1P,E11.4E2)", -2.5,  "-2.5000E+00") ! -2.5001E+00
    call checkfmt("(RU,1P,G6.0E2)", 2.0,  "2.E+00")   ! 3.E+00
    call checkfmt("(RD,1P,G7.0E2)", -2.0,  "-2.E+00") ! -3.E+00
    call checkfmt("(RU,1P,G10.4E2)", 2.3456e5,  "2.3456E+05") ! 2.3457E+05
    call checkfmt("(RD,1P,G11.4E2)", -2.3456e5,  "-2.3456E+05") ! -2.3457E+05

    call checkfmt("(RD,F17.0)", 2.5,     "               2.")
    call checkfmt("(RD,-1P,F17.1)", 2.5, "              0.2")
    call checkfmt("(RD,E17.1)", 2.5,     "          0.2E+01")
    call checkfmt("(RD,1P,E17.0)", 2.5,  "           2.E+00")
    call checkfmt("(RD,ES17.0)", 2.5,    "           2.E+00")
    call checkfmt("(RD,EN17.0)", 2.5,    "           2.E+00")

    call checkfmt("(RC,F17.0)", 2.5,     "               3.")
    call checkfmt("(RC,-1P,F17.1)", 2.5, "              0.3")
    call checkfmt("(RC,E17.1)", 2.5,     "          0.3E+01")
    call checkfmt("(RC,1P,E17.0)", 2.5,  "           3.E+00")
    call checkfmt("(RC,ES17.0)", 2.5,    "           3.E+00")
    call checkfmt("(RC,EN17.0)", 2.5,    "           3.E+00")

    call checkfmt("(RN,F17.0)", 2.5,     "               2.")
    call checkfmt("(RN,-1P,F17.1)", 2.5, "              0.2")
    call checkfmt("(RN,E17.1)", 2.5,     "          0.2E+01")
    call checkfmt("(RN,1P,E17.0)", 2.5,  "           2.E+00")
    call checkfmt("(RN,ES17.0)", 2.5,    "           2.E+00")
    call checkfmt("(RN,EN17.0)", 2.5,    "           2.E+00")

    call checkfmt("(RZ,F17.0)", 2.5,     "               2.")
    call checkfmt("(RZ,-1P,F17.1)", 2.5, "              0.2")
    call checkfmt("(RZ,E17.1)", 2.5,     "          0.2E+01")
    call checkfmt("(RZ,1P,E17.0)", 2.5,  "           2.E+00")
    call checkfmt("(RZ,ES17.0)", 2.5,    "           2.E+00")
    call checkfmt("(RZ,EN17.0)", 2.5,    "           2.E+00")

    call checkfmt("(RZ,F17.0)", -2.5,     "              -2.")
    call checkfmt("(RZ,-1P,F17.1)", -2.5, "             -0.2")
    call checkfmt("(RZ,E17.1)", -2.5,     "         -0.2E+01")
    call checkfmt("(RZ,1P,E17.0)", -2.5,  "          -2.E+00")
    call checkfmt("(RZ,ES17.0)", -2.5,    "          -2.E+00")
    call checkfmt("(RZ,EN17.0)", -2.5,    "          -2.E+00")

    call checkfmt("(RN,F17.0)", -2.5,     "              -2.")
    call checkfmt("(RN,-1P,F17.1)", -2.5, "             -0.2")
    call checkfmt("(RN,E17.1)", -2.5,     "         -0.2E+01")
    call checkfmt("(RN,1P,E17.0)", -2.5,  "          -2.E+00")
    call checkfmt("(RN,ES17.0)", -2.5,    "          -2.E+00")
    call checkfmt("(RN,EN17.0)", -2.5,    "          -2.E+00")

    call checkfmt("(RC,F17.0)", -2.5,     "              -3.")
    call checkfmt("(RC,-1P,F17.1)", -2.5, "             -0.3")
    call checkfmt("(RC,E17.1)", -2.5,     "         -0.3E+01")
    call checkfmt("(RC,1P,E17.0)", -2.5,  "          -3.E+00")
    call checkfmt("(RC,ES17.0)", -2.5,    "          -3.E+00")
    call checkfmt("(RC,EN17.0)", -2.5,    "          -3.E+00")

    call checkfmt("(RU,E17.1)", nearest(2.0, 1.0),     "          0.3E+01")
    call checkfmt("(RD,E17.1)", nearest(3.0, -1.0),    "          0.2E+01")

contains
    subroutine checkfmt(fmt, x, cmp)
        character(len=*), intent(in) :: fmt
        real, intent(in) :: x
        character(len=*), intent(in) :: cmp
        character(len=20) :: s
        
        write(s, fmt) x
        if (s /= cmp) call abort
        !if (s /= cmp) print "(a,1x,a,' expected: ',1x)", fmt, s, cmp
    end subroutine
end program
