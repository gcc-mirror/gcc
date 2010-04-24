! { dg-do compile }
! PR43832 Missing UNIT in OPEN
  open () ! { dg-error "must have UNIT" }
  open (file="test") ! { dg-error "must have UNIT" }
  end

