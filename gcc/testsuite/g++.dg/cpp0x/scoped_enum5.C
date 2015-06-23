// PR c++/66254
// { dg-do compile { target c++11 } }

namespace boost {
namespace http {

enum class read_state
{
    empty
};

template<class Socket>
class basic_socket
{
public:
    http::read_state read_state() const;

    void async_read_request();

private:
    http::read_state istate;
};

template<class Socket>
read_state basic_socket<Socket>::read_state() const
{
    return istate;
}

template<class Socket>
void basic_socket<Socket>::async_read_request()
{
    read_state::empty;
}

} // namespace boost
} // namespace http
