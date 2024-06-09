#[repr(C)]
union MyUnion {} // { dg-error "unions cannot have zero fields" }
