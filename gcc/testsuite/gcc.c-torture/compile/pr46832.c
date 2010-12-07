double pow(double x, double y);
void foo( double x ) {
   int j = (int) ((pow(x, 2)) < 0.0 ? (pow(x, 2))-0.5 : (pow(x, 2))+0.5);
}
