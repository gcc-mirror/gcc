/* PR c++/92365  */
/* { dg-options "-std=c++98 -Wshadow=compatible-local" } */

class a {
public:
  a(char *);
};
void b() {
  a c(0);
  if (0)
    int c;
}
