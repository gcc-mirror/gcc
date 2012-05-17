// PR c++/44516

struct WebService {  };
struct Server {  };

void addHTTPService(Server const &server,
		    WebService const *http)
{
  server += http; // { dg-error "10:no match for 'operator\\+='" }
}
