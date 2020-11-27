#ifndef RUST_BUFFERED_QUEUE_H
#define RUST_BUFFERED_QUEUE_H

#include <vector>

#include "config.h"
#include "system.h"
// order: config, system

namespace Rust {
    // Buffered queue implementation. Items are of type T, queue source is of type Source.
    template<typename T, typename Source>
    class buffered_queue {
      public:
        // Construct empty queue from Source& src.
        buffered_queue(Source& src) : source(src), start(0), end(0), buffer() {}

        // Returns token at position start + n (i.e. n tokens ahead). 
        T peek(int n) {
            // n should not be behind
            gcc_assert(n >= 0);

            int num_queued_items = end - start;
            int num_items_required = n + 1;

            // if required items go past end of queue, add them to queue
            if (num_items_required > num_queued_items) {
                int num_items_to_read = num_items_required - num_queued_items;

                // if queue length + extra items is larger than buffer size, expand buffer
                if (end + num_items_to_read > (int)buffer.size()) {
                    // Resize the buffer by 1.5x
                    int new_size = (buffer.size() + num_items_to_read);
                    new_size += (new_size >> 1);

                    // create new queue buffer with new size
                    std::vector<T> new_queue(new_size);
                    std::copy(buffer.begin() + start, buffer.begin() + end, new_queue.begin());
                    start = 0;
                    end = num_queued_items;

                    // swap member buffer and new queue buffer
                    std::swap(buffer, new_queue);

                    // validate that buffer is large enough now
                    gcc_assert(end + num_queued_items < (int)buffer.size());
                }

                // iterate through buffer and invoke operator () on source on values past original end
                for (int i = 0; i < num_items_to_read; i++) {
                    buffer[end + i] = source();
                }

                // move end based on additional items added
                end += num_items_to_read;
            }

            gcc_assert(0 <= start);
            gcc_assert(start <= end);
            gcc_assert(end <= (int)buffer.size());

            gcc_assert(start + n < end);

            // return value at start + n in buffer
            return buffer[start + n];
        }

        // Advances start by n (+ 1)?.
        void skip(int n) {
            // Call peek to ensure requested n is actually in queue.
            peek(n);

            // Clear values from start to n (inclusive).
            for (int i = 0; i < (n + 1); i++) {
                // Clear value at index
                buffer[start + i] = T();
            }

            // Move start forward by n + 1; 
            start += (n + 1);

            // Ensure start is not impossible somehow
            gcc_assert(0 <= start);
            gcc_assert(start <= end);

            // Compact buffer if empty
            if (start == end) {
                start = end = 0;
            }
        }

      private:
        // Source of tokens for queue
        Source& source;

        // Begin of range in buffer, inclusive
        int start;
        // End of range in buffer, exclusive
        int end;

        // Queue buffer
        std::vector<T> buffer;
    };
}

#endif