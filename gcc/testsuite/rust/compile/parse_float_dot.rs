// floating point literals can't start with a '.'
// TODO: improve the error message emitted here
const X: f32 = .5; // { dg-error ".*" }
