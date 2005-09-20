! { dg-do compile }
      print precision(1.) ! { dg-error "must be of type default CHARACTER" }
      end
