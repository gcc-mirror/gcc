/* PR ipa/124462 */
/* { dg-options "-O -fcondition-coverage" }  */
/* { dg-do compile } */

/* PR ipa/124462 is really a gcov/instrumentation bug.  This program creates
   blocks with multiple incoming edges where one is complex, the exception ->
   destructor, which isn't really the outcome of a conditional expression.  */

void deallocate(char *, long);
struct buffer {};
void vformat_to(buffer, int, int);
enum { inline_buffer_size };
template <unsigned long = inline_buffer_size>
struct basic_memory_buffer : buffer {
  ~basic_memory_buffer() {
    char *data;
    if (data)
      deallocate(data, 0);
  }
};
template <unsigned long SIZE> int to_string(basic_memory_buffer<SIZE>);
int vformat_fmt, vformat_args;
void vformat() {
  basic_memory_buffer<> buffer;
  vformat_to(buffer, vformat_fmt, vformat_args);
  to_string(buffer);
}

/* { dg-final { run-gcov pr124462.C } } */
