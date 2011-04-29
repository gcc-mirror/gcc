! { dg-do run }
! PR48615 Invalid UP/DOWN rounding with E and ES descriptors
! Test case provided by Thomas Henlich.
program pr48615
    call checkfmt("(RU,F17.0)", 2.5,     "               3.")
    call checkfmt("(RU,-1P,F17.1)", 2.5, "              0.3")
    call checkfmt("(RU,E17.1)", 2.5,     "          0.3E+01") ! 0.2E+01
    call checkfmt("(RU,1P,E17.0)", 2.5,  "           3.E+00")
    call checkfmt("(RU,ES17.0)", 2.5,    "           3.E+00") ! 2.E+00
    call checkfmt("(RU,EN17.0)", 2.5,    "           3.E+00")

    call checkfmt("(RD,F17.0)", 2.5,     "               2.")
    call checkfmt("(RD,-1P,F17.1)", 2.5, "              0.2")
    call checkfmt("(RD,E17.1)", 2.5,     "          0.2E+01")
    call checkfmt("(RD,1P,E17.0)", 2.5,  "           2.E+00")
    call checkfmt("(RD,ES17.0)", 2.5,    "           2.E+00")
    call checkfmt("(RD,EN17.0)", 2.5,    "           2.E+00")

    call checkfmt("(RC,F17.0)", 2.5,     "               3.")
    call checkfmt("(RC,-1P,F17.1)", 2.5, "              0.3")
    call checkfmt("(RC,E17.1)", 2.5,     "          0.3E+01") ! 0.2E+01
    call checkfmt("(RC,1P,E17.0)", 2.5,  "           3.E+00")
    call checkfmt("(RC,ES17.0)", 2.5,    "           3.E+00") ! 2.E+00
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
    call checkfmt("(RC,E17.1)", -2.5,     "         -0.3E+01") ! -0.2E+01
    call checkfmt("(RC,1P,E17.0)", -2.5,  "          -3.E+00")
    call checkfmt("(RC,ES17.0)", -2.5,    "          -3.E+00") ! -2.E+00
    call checkfmt("(RC,EN17.0)", -2.5,    "          -3.E+00")

    call checkfmt("(RU,E17.1)", nearest(2.0, 1.0),     "          0.3E+01") ! 0.2E+01
    call checkfmt("(RD,E17.1)", nearest(3.0, -1.0),    "          0.2E+01") ! 0.3E+01

contains
    subroutine checkfmt(fmt, x, cmp)
        character(len=*), intent(in) :: fmt
        real, intent(in) :: x
        character(len=*), intent(in) :: cmp
        character(len=40) :: s
        
        write(s, fmt) x
        if (s /= cmp) call abort
        !if (s /= cmp) print "(a,1x,a,' expected: ',1x)", fmt, s, cmp
    end subroutine
end program
