package main

func subr(p int) int {
  return p
}

func main() {
  if subr(0) != 0 { panic(0) }
}
