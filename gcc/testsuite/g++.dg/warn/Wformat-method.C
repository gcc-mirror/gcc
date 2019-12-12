// { dg-options "-Wformat -fdiagnostics-show-caret" }

class logger
{
public:
  void log (const char *fmt, ...) __attribute__((format (printf, 2, 3)));
};

void test ()
{
  logger out;
  out.log ("before %s after", 42); // { dg-warning "argument 3 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   out.log ("before %s after", 42);
                    ~^         ~~
                     |         |
                     char*     int
                    %d
   { dg-end-multiline-output "" } */
}

template <typename T>
class logger_2
{
public:
  void log (const char *fmt, ...) __attribute__((format (printf, 2, 3)));
};

void test_2 ()
{
  logger_2<int> out;
  out.log ("before %s after", 42); // { dg-warning "argument 3 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   out.log ("before %s after", 42);
                    ~^         ~~
                     |         |
                     char*     int
                    %d
   { dg-end-multiline-output "" } */
}
