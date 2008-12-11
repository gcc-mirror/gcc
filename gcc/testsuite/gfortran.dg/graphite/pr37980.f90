! { dg-options "-O2 -floop-block" }

module INT_MODULE
contains
  pure function spher_cartesians(in1) result(out1)
    integer(kind=kind(1)) :: in1 
    intent(in) :: in1 
    real(kind=kind(1.0d0)), dimension(0:in1,0:in1,0:in1) :: mat0
    mat0 = 0.0d0
  end function spher_cartesians
end module INT_MODULE
