// PR c++/52596

struct msgpack_zone_finalizer_array {
    int* tail;
};
struct msgpack_zone {
    msgpack_zone_finalizer_array finalizer_array;
};
struct zone : public msgpack_zone {
    template <typename T> T* allocate();

};
template <typename T>
T* zone::allocate()
{
  --msgpack_zone::finalizer_array.tail;
}
