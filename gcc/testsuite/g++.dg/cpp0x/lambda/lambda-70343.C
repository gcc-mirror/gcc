// PR c++/70343
// { dg-do run { target c++11 } }

struct Empty{};

template<class T>
struct Data{
    int x;
    float y;

    int properties_parcel4[10];

    Empty j = [&](){
		int i = 10;
                properties_parcel4[0] = i;
		return Empty();
    }();
};

int main () {
    Data<int> k;

  return 0;
}
