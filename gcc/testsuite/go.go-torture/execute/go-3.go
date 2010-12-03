package main

type I interface { send(chan <- int) }

type S struct { v int }
func (p *S) send(c chan <- int) { c <- p.v }

func main() {
  s := S{0};
  var i I = &s;
  c := make(chan int);
  go i.send(c);
  if <- c != 0 { panic(0) }
}
