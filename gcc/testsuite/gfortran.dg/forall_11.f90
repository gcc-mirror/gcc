! { dg-do compile }
! PR 25076
! We erroneously accepted it when a FORALL index was used in a triplet
! specification within the same FORALL header
INTEGER :: A(10,10)
FORALL(I=1:10,J=I:10) ! { dg-error "FORALL index 'i' may not appear in triplet specification" }
  A(I,J)=I+J
ENDFORALL

forall (i=1:10, j=1:i)  ! { dg-error "FORALL index 'i' may not appear in triplet specification" }
   a(i,j) = 5
end forall

forall (i=1:10, j=1:10:i)  ! { dg-error "FORALL index 'i' may not appear in triplet specification" }
   a(i,j) = i - j
end forall

forall (i=i:10) ! { dg-error "FORALL index 'i' may not appear in triplet specification" }
   forall (j=1:j:i)  !  { dg-error "FORALL index 'j' may not appear in triplet specification" }
      a(i,j) = i*j
   end forall
end forall

forall (i=1:10:i) ! { dg-error "FORALL index 'i' may not appear in triplet specification" }
   a(1,i) = 2
end forall

forall (i=1:10)
   forall (j=i:10)
      a(i,j) = i*j
   end forall
end forall
END
