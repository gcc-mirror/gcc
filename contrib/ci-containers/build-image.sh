#!/usr/bin/env bash
#
# Build a container using buildah
#
set -euo pipefail

usage() {
    cat <<EOF
Usage: build-image.sh -d <directory> -t <tag> [-e timestamp] [-- buildah-args...]

Options:
  -d, --dir <path>	Directory with the Containerfile (required).
  -t, --tag <tag>	Tag to apply to the built image (required).
  -e, --epoch <ts>	Set the "created" timestamp for the built image to this number of seconds since the epoch (optional).
			Default is to use the timestamp of the current commit.
			Needs buildah 1.41 or newer.
  -h, --help		Show this help message and exit.

All arguments after a double-dash (--) are forwarded unchanged to 'buildah'.

Example:
  ./build-image.sh -d src -t v1.0 -- --layers --no-cache
EOF
    exit 1
}

DIR=""
TAG=""
EXTRA_ARGS=()

while (( "$#" )); do
    case "$1" in
        -d|--dir)
            if [[ -n "${2-}" ]]; then
                DIR="$2"
                shift 2
            else
                echo "error: --dir requires a value" >&2
                exit 1
            fi
            ;;
        -t|--tag)
            if [[ -n "${2-}" ]]; then
                TAG="$2"
                shift 2
            else
                echo "error: --tag requires a value" >&2
                exit 1
            fi
            ;;
        -e|--epoch)
            if [[ -n "${2-}" ]]; then
                SOURCE_DATE_EPOCH="$2"
                shift 2
            else
                echo "error: --source-date-epoch requires a value" >&2
                exit 1
            fi
            ;;
        -h|--help)
            usage
            ;;
        --)
            shift
            EXTRA_ARGS+=("$@")
            break
            ;;
        *)
            echo "error: unknown option '$1'" >&2
            usage
            ;;
    esac
done

if [[ -z "$DIR" ]]; then
    echo "error: directory (-d/--dir) is required" >&2
    usage
fi

if [[ -z "$TAG" ]]; then
    echo "error: Tag (-t/--tag) is required." >&2
    usage
fi

if [[ ! -e "${DIR}/Containerfile" ]]; then
    echo "error: '${DIR}/Containerfile' does not exist." >&2
    usage
fi

CONTAINER="$(basename "$DIR")"
IMAGE_TAG="${CONTAINER}:${TAG}"

if [[ -z "${SOURCE_DATE_EPOCH-}" ]]; then
    SCRIPT_DIR="$(dirname "$0")"
    SOURCE_DATE_EPOCH="$(cd "${SCRIPT_DIR}" && git log -1 --pretty=%ct)"
fi
export SOURCE_DATE_EPOCH


buildah build \
    -f "${DIR}/Containerfile" \
    -t "$IMAGE_TAG" \
    "${EXTRA_ARGS[@]}" \
    "$DIR"
