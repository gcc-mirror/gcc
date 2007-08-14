! { dg-do compile }
module a

  type, private, bind(C) b ! { dg-error "Expected :: in TYPE definition" }
    integer i
  end type b ! { dg-error "Expecting END MODULE statement" }

  type, public c ! { dg-error "Expected :: in TYPE definition" }
    integer j
  end type c ! { dg-error "Expecting END MODULE statement" }

  type, private d ! { dg-error "Expected :: in TYPE definition" }
    integer k
  end type b ! { dg-error "Expecting END MODULE statement" }

  type, bind(C), public e ! { dg-error "Expected :: in TYPE definition" }
    integer l
  end type e ! { dg-error "Expecting END MODULE statement" }

  type, bind(C) f ! { dg-error "Expected :: in TYPE definition" }
    integer m
  end type f ! { dg-error "Expecting END MODULE statement" }

end module a
