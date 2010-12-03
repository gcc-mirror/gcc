package main
type s struct { i int };
func (v *s) val() int { return v.i }
func main() {
  p := new(s);
  if p.val() != 0 { panic(0) }
}
