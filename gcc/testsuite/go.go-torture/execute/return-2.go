package main

func fn() (i, j int) {
     i = 1;
     j = 2;
     return;
}

func main() {
  var i, j = fn();
  var ret int;
  if i == 1 && j == 2 {
    ret = 0;
  } else {
    ret = 1;
  }
  if ret != 0 { panic(0) }
}
