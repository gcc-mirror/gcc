! { dg-do compile }
! { dg-options "-Wunused-label" }
! PR 26277
! We used to give an incorect warning about label 99 not being referenced
  open(unit=12,err=99)
99 print *,"could not open file ..."
98 continue ! { dg-warning "Label 98 .* defined but not used" }
  close(unit=12,status="delete")
end
