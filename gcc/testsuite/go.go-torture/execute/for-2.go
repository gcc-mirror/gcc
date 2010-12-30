package main

func f1() {
  j := 0;
  for i := 0; i < 10; i++ {
    if i > 2 {
      break;
    }
    j = i;
  }
  if (j != 2) {
    panic(0);
  }
}

func f2() {
  for i := 0; i < 10; i++ {
    if i >= 0 {
      continue;
    }
    panic(1);
  }
}

func f3() {
  lab1:
  for i := 0; i < 10; i++ {
    for j := 0; j < 10; j++ {
      if j > 2 {
        break lab1;
      }
    }
    panic(2);
  }
}

func f4() {
  lab1:
  for i := 0; i < 10; i++ {
    for j := 0; j < 10; j++ {
      continue lab1;
    }
    panic(3);
  }
}

func main() {
  f1();
  f2();
  f3();
  f4()
}
