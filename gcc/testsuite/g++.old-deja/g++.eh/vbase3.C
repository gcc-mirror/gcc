// Build don't link:
// Used to use -fsjlj-exceptions, but that isn't an option anymore.
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
