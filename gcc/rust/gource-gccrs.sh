#!/usr/bin/env bash
set -e

usage() { echo "Usage: $0 [-o output.mp4] [-r 1600x1080]" 1>&2; }

RESOLUTION="1600x1080"
output=""
while getopts ho:r: flag; do
    case "${flag}" in
        o)
            output=${OPTARG}
            ;;
        r)
            RESOLUTION=${OPTARG}
            ;;
        h)
            usage
            exit 0
            ;;
        *)
            usage
            exit 1
            ;;
    esac
done

# we require gource and ffmpeg to generate this stuff
if ! [ -x "$(command -v git)" ]; then
  echo 'Error: git is not installed.' >&2
  exit 1
fi
if ! [ -x "$(command -v gource)" ]; then
  echo 'Error: gource is not installed.' >&2
  exit 1
fi
if ! [ -x "$(command -v ffmpeg)" ]; then
  echo 'Error: ffmpeg is not installed.' >&2
  exit 1
fi

# are we in the root of the repo?
if [ ! -d ".git" ]; then
    echo ".git directory not found, this script must be run at root repo"
    exit 1
fi

# if the output already exists then we should stop
if test -f "$output"; then
    echo "$output already exists."
    exit 1
fi

# check if .ppm exists
if test -f "gource.ppm"; then
    echo "gource.ppm already exists."
    exit 1
fi

# is cargo-gccrs here?
if [ ! -d "cargo-gccrs" ]; then
    git clone git@github.com:Rust-GCC/cargo-gccrs.git
fi

# get the log file for cargo gccrs
pushd cargo-gccrs
rm -f cargo.log
gource --output-custom-log cargo.log .
sed -i -E "s#(.+)\|#\1|/cargo#" cargo.log
mv cargo.log ../
popd

# generate the log file for gccrs
git log --pretty=format:user:%aN%n%ct \
    --reverse \
    --raw \
    --encoding=UTF-8 \
    --no-renames \
    --no-show-signature \
    -- \
    .github \
    Dockerfile \
    *.md \
    gcc/rust \
    gcc/testsuite/rust \
    gcc/testsuite/rust.test > gccrs-raw.log
gource --log-format git --output-custom-log gccrs.log - < gccrs-raw.log
rm -f gccrs-raw.log

# combine them
combined_log="$(mktemp /tmp/gource.XXXXXX)"
cat cargo.log gccrs.log | sort -n > "$combined_log"
rm -f gccrs.log
rm -f cargo.log

echo "Committers:"
awk -F\| {'print  $2'} < "$combined_log"| sort | uniq
echo "======================"

GOURCE_RESOLUTION_ARG="-$RESOLUTION"
GOURCE_OUTPUT_ARGS=""
if [ -n "$output" ]; then
    GOURCE_OUTPUT_ARGS="--output-ppm-stream gource.ppm"
fi

# actually generate the gource
gource "$combined_log" \
       --title "GCC Rust" \
       -s 0.4 \
       -i 0 \
       "$GOURCE_RESOLUTION_ARG" \
       --highlight-users \
       --highlight-dirs \
       --file-extensions \
       --hide mouse \
       --key \
       --stop-at-end \
       "$GOURCE_OUTPUT_ARGS"

# cleanup the log
rm -f "$combined_log"

# transcode it
if [ -n "$output" ]; then
    # this seems to encode at a high quality but ends up being many gb in size
    ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i gource.ppm -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -crf 1 -threads 0 -bf 0 "$output"
fi

echo "crabby on film: $output"
echo "(\/)(º～º)(\/)"
