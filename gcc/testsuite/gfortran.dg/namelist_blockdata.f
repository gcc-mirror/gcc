! { dg-do compile }
! Tests fix for PR21565 - object cannot be in namelist and block data.
      block data
      common /foo/ a
      namelist /foo_n/ a ! { dg-error "not allowed in BLOCK DATA" }
      data a /1.0/
      end
