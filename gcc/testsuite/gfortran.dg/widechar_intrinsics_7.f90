! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1, len=10) :: s1, t1
  character(kind=4, len=10) :: s4, t4

  call check1("foobargeefoobargee", "arg", &
              [ index  ("foobargeefoobargee", "arg", .true.),  &
                index  ("foobargeefoobargee", "arg", .false.), &
                scan   ("foobargeefoobargee", "arg", .true.),  &
                scan   ("foobargeefoobargee", "arg", .false.), &
                verify ("foobargeefoobargee", "arg", .true.),  &
                verify ("foobargeefoobargee", "arg", .false.) ], &
              4_"foobargeefoobargee", 4_"arg", &
              [ index  (4_"foobargeefoobargee", 4_"arg", .true.),  &
                index  (4_"foobargeefoobargee", 4_"arg", .false.), &
                scan   (4_"foobargeefoobargee", 4_"arg", .true.),  &
                scan   (4_"foobargeefoobargee", 4_"arg", .false.), &
                verify (4_"foobargeefoobargee", 4_"arg", .true.),  &
                verify (4_"foobargeefoobargee", 4_"arg", .false.) ])

  call check1("foobargeefoobargee", "", &
              [ index  ("foobargeefoobargee", "", .true.),  &
                index  ("foobargeefoobargee", "", .false.), &
                scan   ("foobargeefoobargee", "", .true.),  &
                scan   ("foobargeefoobargee", "", .false.), &
                verify ("foobargeefoobargee", "", .true.),  &
                verify ("foobargeefoobargee", "", .false.) ], &
              4_"foobargeefoobargee", 4_"", &
              [ index  (4_"foobargeefoobargee", 4_"", .true.),  &
                index  (4_"foobargeefoobargee", 4_"", .false.), &
                scan   (4_"foobargeefoobargee", 4_"", .true.),  &
                scan   (4_"foobargeefoobargee", 4_"", .false.), &
                verify (4_"foobargeefoobargee", 4_"", .true.),  &
                verify (4_"foobargeefoobargee", 4_"", .false.) ])
  call check1("foobargeefoobargee", "klm", &
              [ index  ("foobargeefoobargee", "klm", .true.),  &
                index  ("foobargeefoobargee", "klm", .false.), &
                scan   ("foobargeefoobargee", "klm", .true.),  &
                scan   ("foobargeefoobargee", "klm", .false.), &
                verify ("foobargeefoobargee", "klm", .true.),  &
                verify ("foobargeefoobargee", "klm", .false.) ], &
              4_"foobargeefoobargee", 4_"klm", &
              [ index  (4_"foobargeefoobargee", 4_"klm", .true.),  &
                index  (4_"foobargeefoobargee", 4_"klm", .false.), &
                scan   (4_"foobargeefoobargee", 4_"klm", .true.),  &
                scan   (4_"foobargeefoobargee", 4_"klm", .false.), &
                verify (4_"foobargeefoobargee", 4_"klm", .true.),  &
                verify (4_"foobargeefoobargee", 4_"klm", .false.) ])
  call check1("foobargeefoobargee", "gee", &
              [ index  ("foobargeefoobargee", "gee", .true.),  &
                index  ("foobargeefoobargee", "gee", .false.), &
                scan   ("foobargeefoobargee", "gee", .true.),  &
                scan   ("foobargeefoobargee", "gee", .false.), &
                verify ("foobargeefoobargee", "gee", .true.),  &
                verify ("foobargeefoobargee", "gee", .false.) ], &
              4_"foobargeefoobargee", 4_"gee", &
              [ index  (4_"foobargeefoobargee", 4_"gee", .true.),  &
                index  (4_"foobargeefoobargee", 4_"gee", .false.), &
                scan   (4_"foobargeefoobargee", 4_"gee", .true.),  &
                scan   (4_"foobargeefoobargee", 4_"gee", .false.), &
                verify (4_"foobargeefoobargee", 4_"gee", .true.),  &
                verify (4_"foobargeefoobargee", 4_"gee", .false.) ])
  call check1("foobargeefoobargee", "foo", &
              [ index  ("foobargeefoobargee", "foo", .true.),  &
                index  ("foobargeefoobargee", "foo", .false.), &
                scan   ("foobargeefoobargee", "foo", .true.),  &
                scan   ("foobargeefoobargee", "foo", .false.), &
                verify ("foobargeefoobargee", "foo", .true.),  &
                verify ("foobargeefoobargee", "foo", .false.) ], &
              4_"foobargeefoobargee", 4_"foo", &
              [ index  (4_"foobargeefoobargee", 4_"foo", .true.),  &
                index  (4_"foobargeefoobargee", 4_"foo", .false.), &
                scan   (4_"foobargeefoobargee", 4_"foo", .true.),  &
                scan   (4_"foobargeefoobargee", 4_"foo", .false.), &
                verify (4_"foobargeefoobargee", 4_"foo", .true.),  &
                verify (4_"foobargeefoobargee", 4_"foo", .false.) ])

  call check1("  \b fe \b\0 bar cad", " \b\0", &
              [ index  ("  \b fe \b\0 bar cad", " \b\0", .true.),  &
                index  ("  \b fe \b\0 bar cad", " \b\0", .false.), &
                scan   ("  \b fe \b\0 bar cad", " \b\0", .true.),  &
                scan   ("  \b fe \b\0 bar cad", " \b\0", .false.), &
                verify ("  \b fe \b\0 bar cad", " \b\0", .true.),  &
                verify ("  \b fe \b\0 bar cad", " \b\0", .false.) ], &
              4_"  \uC096 fe \uC096\uB8DE bar cad", 4_" \uC096\uB8DE", &
              [ index  (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .true.),  &
                index  (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .false.), &
                scan   (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .true.),  &
                scan   (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .false.), &
                verify (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .true.),  &
                verify (4_"  \uC096 fe \uC096\uB8DE bar cad", &
                        4_" \uC096\uB8DE", .false.) ])

contains

  subroutine check1 (s1, t1, res1, s4, t4, res4)
    character(kind=1, len=*) :: s1, t1
    character(kind=4, len=*) :: s4, t4
    integer :: res1(6), res4(6)

    if (any (res1 /= res4)) STOP 1

    if (index  (s1, t1, .true.)  /= res1(1)) STOP 2
    if (index  (s1, t1, .false.) /= res1(2)) STOP 3
    if (scan   (s1, t1, .true.)  /= res1(3)) STOP 4
    if (scan   (s1, t1, .false.) /= res1(4)) STOP 5
    if (verify (s1, t1, .true.)  /= res1(5)) STOP 6
    if (verify (s1, t1, .false.) /= res1(6)) STOP 7

    if (index  (s4, t4, .true.)  /= res4(1)) STOP 8
    if (index  (s4, t4, .false.) /= res4(2)) STOP 9
    if (scan   (s4, t4, .true.)  /= res4(3)) STOP 10
    if (scan   (s4, t4, .false.) /= res4(4)) STOP 11
    if (verify (s4, t4, .true.)  /= res4(5)) STOP 12
    if (verify (s4, t4, .false.) /= res4(6)) STOP 13

  end subroutine check1

end
