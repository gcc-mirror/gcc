module imports.test10736c;

struct Chunks(Source)
{
    this(Source source, size_t chunkSize)
    {
        _source = source;
        _chunkSize = chunkSize;
    }

    typeof(this) opSlice(size_t, size_t)
    {
        return chunks(_source, _chunkSize);
    }

private:
    Source _source;
    size_t _chunkSize;
}

Chunks!Source chunks(Source)(Source source, size_t chunkSize)
{
    return typeof(return)(source, chunkSize);
}
