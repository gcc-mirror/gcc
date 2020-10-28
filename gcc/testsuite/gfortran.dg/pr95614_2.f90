! { dg-do compile }

module m        ! { dg-error ".1." }
  common /xc/ m ! { dg-error "cannot appear in a COMMON" }
end

