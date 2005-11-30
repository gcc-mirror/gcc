! { dg-do compile }
! Tests fix for PR24223 - ICE on equivalence statement.
!
module FLAGS
  character(len=5) :: Encodings
  character :: at, dev
  equivalence ( encodings(1:1),at ), ( encodings(2:2),dev)
end module FLAGS
