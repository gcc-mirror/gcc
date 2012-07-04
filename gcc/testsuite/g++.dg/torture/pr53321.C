// { dg-do compile }
// { dg-require-profiling "-fprofile-generate" }
// { dg-options "-fprofile-generate" }

typedef long unsigned int size_t;

extern "C"
{
  extern void *memcpy (void *__dest, __const void *__src, size_t __n);
}

extern char *src, *sources;
extern int n_sources;

static void
find_source (const char *file_name)
{
  memcpy (src, sources, n_sources * sizeof (*sources));
}

extern const char *gcov_read_string (void);

static void read_graph_file (void)
{
  find_source (gcov_read_string ());
}

static void process_file (void)
{
  read_graph_file ();
}

int main ()
{
  process_file ();
}
