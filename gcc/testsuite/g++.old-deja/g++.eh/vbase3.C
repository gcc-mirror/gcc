// Build don't link:
// Special g++ Options: -fsjlj-exceptions
// Origin: Donn Terry <donn@interix.com>

struct ios {
  virtual ~ios();
};
struct fstreambase : virtual public ios {
  fstreambase();
};
fstreambase::fstreambase()
{
}
