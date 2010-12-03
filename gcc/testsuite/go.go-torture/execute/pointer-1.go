package main

func main() {
  p := new(int);
  *p = 0;
  if *p != 0 { panic(0) }
}
