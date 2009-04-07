typedef void fn() const;

fn* fp;			 // { dg-error "pointer.*qualified function type" }
